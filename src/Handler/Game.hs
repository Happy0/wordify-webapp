{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Handler.Game where

import Control.Concurrent
import Control.Error (ExceptT (..), lastDef, note, runExceptT)
import Controllers.Chat.Chatroom (subscribeMessagesLive)
import qualified Controllers.Chat.Chatroom as CR (ChatMessage (ChatMessage), Chatroom)
import Controllers.Common.CacheableSharedResource (getCacheableResource)
import qualified Controllers.Definition.DefinitionService as D
import Controllers.Game.Api
import Controllers.Game.Api (initialSocketMessage)
import Controllers.Game.Game
import Controllers.Game.GameDefinitionController (GameDefinitionController, getStoredDefinitions)
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Persist
import Controllers.User.Model.AuthUser
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Either (fromRight)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Monoid as M
import Data.Pool
import qualified Data.Text as T
import Data.Text.Read (decimal, rational)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql
import Import
import InactivityTracker
import Model.Api
import qualified Network.WebSockets.Connection as C
import Repository.DefinitionRepository (DefinitionRepositoryImpl, GameWordItem (GameWordItem), WordDefinitionItem (WordDefinitionItem), getGameDefinitionsImpl)
import Util.ConduitChan (chanSource)
import Wordify.Rules.Board
import Wordify.Rules.Dictionary
import qualified Wordify.Rules.Game as G
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import Wordify.Rules.Player (Player (endBonus))
import qualified Wordify.Rules.Player as P
import Yesod.WebSockets
import Handler.Model.ClientGame (fromServerPlayer, fromServerTile, fromServerMoveHistory)
import qualified Foundation as G
import qualified Wordify.Rules.Move as G
import Model.GameSetup (LocalisedGameSetup(..))
import Wordify.Rules.Game (Game(..))
import qualified Data.List.Split.Internals as T

getGameR :: Text -> Handler Html
getGameR gameId = do
  app <- getYesod
  liftIO $ trackRequestReceivedActivity (inactivityTracker app)
  maid <- maybeAuthId

  maybeUser <- case maid of
    Nothing -> return Nothing
    Just _ -> do
      authedUser <- requireUsername
      return $ Just (AuthUser (authenticatedUserId authedUser) Nothing)

  {-- If this is a websocket request, the handler is short cutted here
      Once the client has loaded the page and javascript, the javascript for the page
      initiates the websocket request which arrives here -}
  webSockets $ gameApp app gameId maybeUser

  runResourceT $ do
    (_, game) <- getCacheableResource (games app) gameId
    lift $ renderGamePage app gameId maybeUser game

getServerPlayers :: ServerGame -> STM [ServerPlayer]
getServerPlayers serverGame = do
  snapshot <- makeServerGameSnapshot serverGame
  pure $ snapshotPlayers snapshot

chatMessageSinceQueryParamValue :: Handler (Maybe Int)
chatMessageSinceQueryParamValue = do
  queryParamValue <- lookupGetParam "chatMessagesSinceMessageNumber"
  let maybeMessageNumber = note "No chat message since param specific" queryParamValue >>= decimal
  case maybeMessageNumber of
    Left _ -> pure Nothing
    Right (messageNumber, _) -> pure (Just messageNumber)

definitionsSinceQueryParamValue :: Handler (Maybe Int)
definitionsSinceQueryParamValue = do
  queryParamValue <- lookupGetParam "definitionsSinceMessageNumber"
  let maybeMessageNumber = note "No chat message since param specific" queryParamValue >>= decimal
  case maybeMessageNumber of
    Left _ -> pure Nothing
    Right (messageNumber, _) -> pure (Just messageNumber)


gamePagelayout :: Widget -> Handler Html
gamePagelayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>Wordify
                    <meta charset="UTF-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    <link rel="manifest" href="/manifest.json">
                    ^{pageHead pc}
                <body>
                    <div .special-wrapper>
                        ^{pageBody pc}
        |]

renderGamePage :: App -> Text -> Maybe AuthUser -> Either Text ServerGame -> Handler Html
renderGamePage _ _ _ (Left err) = invalidArgs [err]
renderGamePage app gameId maybeUser (Right serverGame) = do
  let isLoggedIn = isJust maybeUser

  let maybePlayerNumber = maybeUser >>= getPlayerNumber serverGame

  gameSoFar <- liftIO (readTVarIO (game serverGame))

  let rack = map fromServerTile . P.tilesOnRack <$> (maybePlayerNumber >>= G.getPlayer gameSoFar)
  let playersGameState = G.players gameSoFar
  let playing = G.playerNumber gameSoFar
  let numTilesRemaining = bagSize (G.bag gameSoFar)
  let gameMoveSummaries = gameToMoveSummaries gameSoFar

  let gameOver = G.gameStatus gameSoFar == G.Finished

  serverPlayers <- atomically $ getServerPlayers serverGame
  let clientPlayers = zipWith (fromServerPlayer gameOver) serverPlayers playersGameState

  let tileValues = (tileLettersToValueMap . gameSetup) serverGame
  let boardString = textRepresentation (board gameSoFar)

  case gameMoveSummaries of
    Left err -> invalidArgs [err]
    Right _ -> liftIO (return ())

  let summaries = fromServerMoveHistory (fromRight [] gameMoveSummaries)

  gamePagelayout $ do
    addStylesheet $ StaticR wordifyCss
    addScript $ StaticR wordifyJs
    toWidget
      [julius|

              var url = document.URL;
              var webSocketUrl = url.replace("http:", "ws:").replace("https:", "wss:");

              var initialState = {
                myPlayerNumber: #{toJSON maybePlayerNumber},
                playerToMove: #{toJSON playing},
                players: #{toJSON clientPlayers},
                moveHistory: #{toJSON summaries},
                tilesRemaining: #{toJSON numTilesRemaining},
                potentialScore: null,
                lastMoveReceived: Date.now(),

                // Let the websocket deal with these
                chatMessages: [],
                lastChatMessageReceived: 0,
                lastDefinitionReceived: 0,
                rack: #{toJSON rack},
                boardLayout: Wordify.BOARD_LAYOUT,
                boardString: #{toJSON boardString},
                tileValues: #{toJSON tileValues},
                gameEnded: false
              }
              
              const game = Wordify.createRound('#wordifyround', {
                initialState: initialState,
                websocketUrl: webSocketUrl,
                gameId: #{toJSON gameId},
                isLoggedIn: #{toJSON isLoggedIn},
                vapidPublicKey: #{toJSON (vapidPublicKey app)}
              });
             
          |]
    [whamlet|
          <div #wordifyround>
      |]

subscribeChatGameMessages :: CR.Chatroom -> Maybe Int -> ConduitT () GameMessage IO ()
subscribeChatGameMessages chatroom since = subscribeMessagesLive chatroom since .| CL.map toGameMessage
  where
    toGameMessage :: CR.ChatMessage -> GameMessage
    toGameMessage (CR.ChatMessage _ displayName chatMessage sentTime messageNumber) =
      PlayerChat (Controllers.Game.Api.ChatMessage displayName chatMessage sentTime messageNumber)

subscribeGameMessages :: TChan GameMessage -> ConduitT () GameMessage IO ()
subscribeGameMessages = chanSource

handleBroadcastMessages :: C.Connection -> TChan GameMessage -> CR.Chatroom -> Maybe Int -> IO ()
handleBroadcastMessages connection serverGame chatroom chatMessagesSince = do
  let chatMessagesSubscription = subscribeChatGameMessages chatroom chatMessagesSince .| CL.map toJSONResponse .| CL.mapM_ (liftIO . C.sendTextData connection)
  let gameMessagesSubscription = subscribeGameMessages serverGame .| CL.map toJSONResponse .| CL.mapM_ (liftIO . C.sendTextData connection)

  race_ (runConduit chatMessagesSubscription) (runConduit gameMessagesSubscription)

handleInboundSocketMessages :: App -> C.Connection -> CR.Chatroom -> ServerGame -> Maybe AuthUser -> IO ()
handleInboundSocketMessages app connection chatroom serverGame maybeUser = forever
  $ do
    msg <- C.receiveData connection
    case eitherDecode msg of
      Left err -> C.sendTextData connection $ toJSONResponse (InvalidCommand (pack err))
      Right parsedCommand -> do
        response <- liftIO $ performRequest serverGame chatroom (gameDefinitionController app) (appConnPool app) (userEventChannels app) (pushController app) (userController app) maybeUser parsedCommand
        C.sendTextData connection $ toJSONResponse response

sendInitialGameState :: C.Connection -> ServerGameSnapshot -> Maybe AuthUser -> IO ()
sendInitialGameState connection serverGameSnapshot maybeUser = do
  let initialGameState = initialSocketMessage serverGameSnapshot maybeUser
  mapM_ (C.sendTextData connection . toJSONResponse) initialGameState

handleWebsocket :: App -> C.Connection -> Text -> Maybe AuthUser -> Maybe Int -> Maybe Int -> IO ()
handleWebsocket app connection gameId maybeUser chatMessagesSince definitionsSince = runResourceT $ do
  result <- runExceptT $ do
    serverGame <- ExceptT $ snd <$> getCacheableResource (games app) gameId
    chatId     <- liftIO  $ getChatId gameId maybeUser serverGame
    chatroom   <- ExceptT $ snd <$> getCacheableResource (chatRooms app) chatId
    pure (serverGame, chatroom)

  case result of
    Left err -> liftIO $ C.sendTextData connection (toJSONResponse (InvalidCommand err))
    Right (serverGame, chatroom) -> liftIO $ do
      let inactivityTrackerState = inactivityTracker app
      (channel, gameSnapshot) <- atomically $ (,) <$> dupTChan (broadcastChannel serverGame) <*> makeServerGameSnapshot serverGame
      withTrackWebsocketActivity inactivityTrackerState $ do
        withNotifyJoinAndLeave (appConnPool app) (userEventChannels app) serverGame maybeUser $ do
          sendInitialGameState connection gameSnapshot maybeUser
          sendPreviousDefinitions (gameDefinitionController app) gameId definitionsSince connection
          let handleOutbound = handleBroadcastMessages connection channel chatroom chatMessagesSince
          let handleInbound = handleInboundSocketMessages app connection chatroom serverGame maybeUser
          race_ handleOutbound handleInbound
  where
    getChatId :: Text -> Maybe AuthUser -> ServerGame -> IO Text
    getChatId gameId (Just user) game = do
      playerInGame <- flip playerIsInGame user <$> atomically (makeServerGameSnapshot game)
      if playerInGame then pure gameId else pure (observerChatId gameId)
    getChatId gameId Nothing _ = pure (observerChatId gameId)

    observerChatId :: Text -> Text
    observerChatId gId = T.concat [gId, "#ObserverChatroom"]

gameApp :: App -> Text -> Maybe AuthUser -> WebSocketsT Handler ()
gameApp app gameId maybeUser = do
  connection <- ask
  chatMessagesSinceParam <- lift chatMessageSinceQueryParamValue
  definitionsSinceParam <- lift definitionsSinceQueryParamValue
  liftIO (handleWebsocket app connection gameId maybeUser chatMessagesSinceParam definitionsSinceParam)

gameToMoveSummaries :: G.Game -> Either Text [MoveSummary]
gameToMoveSummaries game =
  if not (null moves)
    then do
      let gameTransitions = restoreGameLazy emptyGame $ NE.fromList (toList moves)
      let reconstructedGameSummaries = mapM (fmap transitionToSummary) $ NE.toList gameTransitions
      case reconstructedGameSummaries of
        Left err -> Left (pack (show err))
        Right summaries -> Right summaries
    else return []
  where
    -- TODO: Add function to make a new empty game from a game history to
    -- wordify
    Right playersState = makeGameStatePlayers (L.length $ G.players game)
    Right emptyGame = G.makeGame playersState originalBag (G.dictionary game)
    (G.History originalBag moves) = G.history game

sendPreviousDefinitions :: GameDefinitionController -> Text -> Maybe Int -> C.Connection -> IO ()
sendPreviousDefinitions gameDefinitionController gameId since connection =
  runConduit $ getStoredDefinitions gameDefinitionController gameId .| CL.filter (isAfterDefinitionNumber since) .| CL.map mapDefinitions .| CL.map toJSONResponse .| CL.mapM_ (liftIO . C.sendTextData connection)
  where
    mapDefinitions :: GameWordItem -> GameMessage
    mapDefinitions (GameWordItem word createdAt definitions definitionNumber) =
      let wordDefinitions = map makeDefinition definitions
       in WordDefinitions word createdAt wordDefinitions definitionNumber

    isAfterDefinitionNumber :: Maybe Int -> GameWordItem -> Bool
    isAfterDefinitionNumber Nothing (GameWordItem _ createdAt _ _) = True
    isAfterDefinitionNumber (Just defNo) (GameWordItem _ _ _ definitionNumber) = definitionNumber > defNo

    makeDefinition :: WordDefinitionItem -> D.Definition
    makeDefinition (WordDefinitionItem partOfSpeech definition example) =
      D.Definition partOfSpeech definition example