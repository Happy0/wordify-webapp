{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Handler.Game where

import Control.Concurrent
import Control.Error (ExceptT (..), lastDef, note, runExceptT)
import Modules.Games.Api (getGame)
import Modules.Chats.Api (ChatService, getChatroom, subscribeMessagesLive, Chatroom, getExistingChatMessages)
import qualified Modules.Chats.Api as CR (ChatMessage (ChatMessage), Chatroom)
import qualified Modules.Definition.DefinitionClient as D
import Controllers.Game.Api
import Controllers.Game.Game
import Controllers.Game.GameDefinitionController (GameDefinitionController, getStoredDefinitions)
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.User.Model.ServerUser (ServerUser (ServerUser), userId)
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Bifunctor (first)
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
import qualified Handler.Common.Chat as HC
import Handler.Common (defaultPageLayout)
import Handler.Common.Chat (sendChatUpdate)
import Handler.Common.ClientNotificationPresentation (notificationsForUser, sendNotificationUpdate, nextNotifUpdateFromUserChan, notificationsWebSocketHandler)
import Controllers.Game.Model.UserEventSubscription (UserEvent (..), NotificationUpdate (..))
import Control.Concurrent.STM (retry)
import qualified Foundation as G
import qualified Wordify.Rules.Move as G
import Model.GameSetup (LocalisedGameSetup(..))
import Wordify.Rules.Game (Game(..))
import qualified Data.List.Split.Internals as T
import Modules.UserEvent.Api (UserEventService)

getGameR :: Text -> Handler Html
getGameR gameId = do
  app <- getYesod
  liftIO $ trackRequestReceivedActivity (inactivityTracker app)
  maid <- maybeAuthId

  maybeUser <- case maid of
    Nothing -> return Nothing
    Just _ -> do
      authedUser <- requireUsername
      return $ Just (ServerUser (authenticatedUserId authedUser) Nothing)

  {-- If this is a websocket request, the handler is short cutted here
      Once the client has loaded the page and javascript, the javascript for the page
      initiates the websocket request which arrives here -}
  webSockets $ gameApp app gameId maybeUser

  runResourceT $ do
    (_, game) <- getGame (gameService app) gameId
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



renderGamePage :: App -> Text -> Maybe ServerUser -> Either Text ServerGame -> Handler Html
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

  notifs <- case maybeUser of
    Nothing   -> return []
    Just user -> liftIO $ notificationsForUser app (userId user)

  defaultPageLayout $ do
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
                vapidPublicKey: #{toJSON (vapidPublicKey app)},
                notifications: #{toJSON notifs}
              });
             
          |]
    [whamlet|
          <div #wordifyround>
      |]

toChatMessage :: CR.ChatMessage -> HC.ChatMessage
toChatMessage (CR.ChatMessage _ displayName chatMessage sentTime messageNumber) =
  HC.ChatMessage displayName chatMessage sentTime messageNumber

subscribeGameMessages :: TChan GameMessage -> ConduitT () GameMessage IO ()
subscribeGameMessages = chanSource

data OutboundMessage = GameMsg GameMessage | ChatMsg HC.ChatMessage | NotifMsg NotificationUpdate

handleBroadcastMessages :: C.Connection -> TChan GameMessage -> CR.Chatroom -> Maybe Int -> TChan UserEvent -> IO ()
handleBroadcastMessages connection liveGameMessages chatroom since userEventChan = do
  -- Subscribing to the channel comes first in case there's a race between doing so and sending the previous messages
  liveChatMessagesChan <- subscribeMessagesLive chatroom
  _ <- sendPreviousChatMessages connection chatroom since
  forever (sendBroadcastMessages connection liveChatMessagesChan liveGameMessages userEventChan)

sendBroadcastMessages :: C.Connection -> TChan CR.ChatMessage -> TChan GameMessage -> TChan UserEvent -> IO ()
sendBroadcastMessages connection chatMessageChannel gameMessageChannel userEventChan = do
  let nextChatMessage = ChatMsg . toChatMessage <$> readTChan chatMessageChannel
  let nextGameMessage = GameMsg <$> readTChan gameMessageChannel
  let nextNotif       = NotifMsg <$> nextNotifUpdateFromUserChan userEventChan
  nextMessage <- atomically (nextGameMessage `orElse` nextChatMessage `orElse` nextNotif)
  case nextMessage of
    GameMsg msg      -> C.sendTextData connection (toJSONResponse msg)
    ChatMsg msg      -> sendChatUpdate connection msg
    NotifMsg update  -> sendNotificationUpdate connection update

handleInboundSocketMessages :: App -> C.Connection -> CR.Chatroom -> ServerGame -> Maybe ServerUser -> IO ()
handleInboundSocketMessages app connection chatroom serverGame maybeUser = forever
  $ do
    msg <- C.receiveData connection
    case eitherDecode msg of
      Left err -> C.sendTextData connection $ toJSONResponse (InvalidCommand (pack err))
      Right parsedCommand -> do
        response <- liftIO $ performRequest serverGame chatroom (gameDefinitionController app) (gameService app) (userEventService app) (notificationService app) (userController app) maybeUser parsedCommand
        C.sendTextData connection $ toJSONResponse response

sendInitialGameState :: C.Connection -> ServerGameSnapshot -> Maybe ServerUser -> IO ()
sendInitialGameState connection serverGameSnapshot maybeUser = do
  let initialGameState = initialSocketMessage serverGameSnapshot maybeUser
  mapM_ (C.sendTextData connection . toJSONResponse) initialGameState

handleWebsocket :: App -> C.Connection -> Text -> Maybe ServerUser -> Maybe Int -> Maybe Int -> TChan UserEvent -> IO ()
handleWebsocket app connection gameId maybeUser chatMessagesSince definitionsSince userEventChan = 
  runResourceT $ do
    resources <- getWebsocketResources
    case resources of
      Left err -> liftIO $ C.sendTextData connection (toJSONResponse (InvalidCommand err))
      Right (serverGame, chatroom) -> liftIO (runWebSocket serverGame chatroom)
    where
    runWebSocket :: ServerGame -> Chatroom -> IO ()
    runWebSocket serverGame chatroom = do
      let inactivityTrackerState = inactivityTracker app
      (channel, gameSnapshot) <- atomically $ (,) <$> dupTChan (broadcastChannel serverGame) <*> makeServerGameSnapshot serverGame
      withTrackWebsocketActivity inactivityTrackerState $ do
        withNotifyJoinAndLeave (gameService app) (userEventService app) serverGame maybeUser $ do
          sendInitialGameState connection gameSnapshot maybeUser
          sendPreviousDefinitions (gameDefinitionController app) gameId definitionsSince connection
          let handleOutbound = handleBroadcastMessages connection channel chatroom chatMessagesSince userEventChan
          let handleInbound = handleInboundSocketMessages app connection chatroom serverGame maybeUser
          race_ handleOutbound handleInbound

    getWebsocketResources :: MonadResource m => m (Either Text (ServerGame, Chatroom))
    getWebsocketResources = runExceptT $ do
      serverGame <- ExceptT $ snd <$> getGame (gameService app) gameId
      chatId     <- liftIO  $ getChatId gameId maybeUser serverGame
      chatroom   <- ExceptT $ getChatroom (chatService app) chatId
      pure (serverGame, chatroom)

    getChatId :: Text -> Maybe ServerUser -> ServerGame -> IO Text
    getChatId gameId (Just user) game = do
      playerInGame <- flip playerIsInGame user <$> atomically (makeServerGameSnapshot game)
      if playerInGame then pure gameId else pure (observerChatId gameId)
    getChatId gameId Nothing _ = pure (observerChatId gameId)

    observerChatId :: Text -> Text
    observerChatId gId = T.concat [gId, "#ObserverChatroom"]

gameApp :: App -> Text -> Maybe ServerUser -> WebSocketsT Handler ()
gameApp app gameId maybeUser = do
  connection <- ask
  chatMessagesSinceParam <- lift chatMessageSinceQueryParamValue
  definitionsSinceParam <- lift definitionsSinceQueryParamValue
  case maybeUser of
    Nothing -> liftIO $ do
      dummyChan <- newBroadcastTChanIO >>= atomically . dupTChan
      handleWebsocket app connection gameId Nothing chatMessagesSinceParam definitionsSinceParam dummyChan
    Just user ->
      notificationsWebSocketHandler app (userId user) $ \userEventChan ->
        liftIO $ handleWebsocket app connection gameId maybeUser chatMessagesSinceParam definitionsSinceParam userEventChan

gameToMoveSummaries :: G.Game -> Either Text [MoveSummary]
gameToMoveSummaries game =
  case NE.nonEmpty (toList moves) of
    Nothing -> Right []
    Just nonEmptyMoves -> do
      playersState <- makeGameStatePlayers (L.length $ G.players game)
      gameTransitions <- first (pack . show) $ G.restoreGameLazy playersState originalBag (G.dictionary game) nonEmptyMoves
      first (pack . show) $ mapM (fmap transitionToSummary) $ NE.toList gameTransitions
  where
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

sendPreviousChatMessages :: C.Connection -> Chatroom -> Maybe Int -> IO ()
sendPreviousChatMessages connection chatroom chatMessagesSince =
    runConduit $ getExistingChatMessages chatroom chatMessagesSince
     .| CL.map toChatMessage
     .| CL.mapM_ (liftIO . sendChatUpdate connection)