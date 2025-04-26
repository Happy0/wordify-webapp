{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Handler.Game where

import Control.Concurrent
import Control.Error (note)
import Controllers.Chat.Model.Chatroom (subscribeMessagesLive)
import qualified Controllers.Chat.Model.Chatroom as CR (ChatMessage (ChatMessage), Chatroom)
import Controllers.Common.CacheableSharedResource (getCacheableResource, withCacheableResource)
import qualified Controllers.Definition.DefinitionService as D
import Controllers.Game.Api
import Controllers.Game.Api (initialSocketMessage)
import Controllers.Game.Game
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Persist
import Controllers.User.Model.AuthUser
import Controllers.User.Persist
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Monoid as M
import Data.Pool
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
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
import Yesod.Core
import Yesod.WebSockets

getGameR :: Text -> Handler Html
getGameR gameId = do
  request <- getRequest
  app <- getYesod
  liftIO $ trackRequestReceivedActivity (inactivityTracker app)
  let cookies = reqCookies request
  userId <- maybeAuthId

  maybeUser <- case userId of
    Nothing -> pure Nothing
    Just user -> liftIO $ getUser (appConnPool app) user

  {-- If this is a websocket request, the handler is short cutted here
      Once the client has loaded the page and javascript, the javascript for the page
      initiates the websocket request which arrives here -}
  webSockets $ gameApp app gameId maybeUser

  runResourceT $ do
    (_, game) <- getCacheableResource (games app) gameId
    lift $ renderGamePage app gameId maybeUser game

getConnectionStatuses :: ServerGame -> STM [ConnectionStatus]
getConnectionStatuses serverGame = do
  snapshot <- makeServerGameSnapshot serverGame
  pure $ connectionStatuses (snapshotPlayers snapshot)

chatMessageSinceQueryParamValue :: Handler (Maybe UTCTime)
chatMessageSinceQueryParamValue = do
  queryParamValue <- lookupGetParam "chatMessagesSince"
  let sinceEpochSeconds = note "No chat message since param specific" queryParamValue >>= (fmap fst . rational)
  case sinceEpochSeconds of
    Right secondsSinceUnixEpoch -> pure $ Just ((posixSecondsToUTCTime . fromRational) secondsSinceUnixEpoch)
    _ -> pure Nothing

renderGamePage :: App -> Text -> Maybe AuthUser -> Either Text ServerGame -> Handler Html
renderGamePage _ _ _ (Left err) = invalidArgs [err]
renderGamePage app gameId maybeUser (Right serverGame) = do
  let maybePlayerNumber = maybeUser >>= getPlayerNumber serverGame

  gameSoFar <- liftIO (readTVarIO (game serverGame))

  let rack = P.tilesOnRack <$> (maybePlayerNumber >>= G.getPlayer gameSoFar)
  let players = G.players gameSoFar
  let playing = G.playerNumber gameSoFar
  let numTilesRemaining = (bagSize (G.bag gameSoFar))
  let gameMoveSummaries = gameToMoveSummaries gameSoFar

  connectionStatuses <- atomically $ getConnectionStatuses serverGame

  case gameMoveSummaries of
    Left err -> invalidArgs [err]
    Right _ -> liftIO (return ())

  let summaries = either (\_ -> []) id gameMoveSummaries

  defaultLayout $ do
    addStylesheet $ (StaticR css_scrabble_css)
    addStylesheet $ (StaticR css_round_css)
    addStylesheet $ (StaticR css_bootstrap_css)
    addScript $ (StaticR js_round_js)
    addScript $ (StaticR js_touchpunch_js)
    toWidget
      [julius|
              jQuery.noConflict();

              var url = document.URL,
              url = url.replace("http:", "ws:").replace("https:", "wss:");
              var conn;

              var opts = {element: document.getElementById("scrabbleground")};
              opts.ground = {}
              opts.ground.board = #{toJSON (G.board gameSoFar)};
              opts.players = #{toJSON players}
              opts.playerNumber = #{toJSON maybePlayerNumber}
              opts.tilesRemaining = #{toJSON numTilesRemaining}
              opts.moveHistory = #{toJSON summaries}
              opts.lastMoveReceived = #{toJSON (G.moveNumber gameSoFar)}
              opts.ground.highlightMoveMainWordClass = "highlight-main";
              opts.connections = #{toJSON connectionStatuses}

              var send = function(objectPayload) {
                  if (!conn || conn.readyState !== WebSocket.OPEN) {
                    return false;
                  }

                  var json = JSON.stringify(objectPayload);
                  conn.send(json);

                  return true;
              }

              opts.send = send;
              var round = Round(opts);

              //TODO: Make a rack be definable in the 'data' object
              round.controller.updateRack(#{toJSON rack});

              round.controller.setPlayerToMove(#{toJSON playing});

              function connectWebsocket() {
                var lastChatMessageReceived = round.controller.getLastChatMessageReceivedSecondsSinceEpoch();

                var url = document.URL + `?chatMessagesSince=${lastChatMessageReceived}`;

                url = url.replace("http:", "ws:").replace("https:", "wss:");
                conn = new WebSocket(url);

                conn.onmessage = function (e) {
                  var data = JSON.parse(e.data);
                  round.socketReceive(data);
                };

                conn.onclose = function(e) {
                  console.log('Socket is closed. Reconnecting in 1 second.', e.reason);
                  setTimeout(function() {
                    connectWebsocket();
                  }, 1000);
                };

                conn.onerror = function(err) {
                  console.error('Socket error: ', err.message, 'Closing.');
                  conn.close();
                };
              }

              connectWebsocket()

              document.addEventListener('DOMContentLoaded', function () {
                if (Notification.permission !== "granted")
                  Notification.requestPermission();
              });
          |]
    [whamlet|
          <div #scrabbleground>
      |]

subscribeChatGameMessages :: CR.Chatroom -> Maybe UTCTime -> ConduitT () GameMessage IO ()
subscribeChatGameMessages chatroom since = subscribeMessagesLive chatroom since .| CL.map toGameMessage
  where
    toGameMessage :: CR.ChatMessage -> GameMessage
    toGameMessage (CR.ChatMessage displayName chatMessage sentTime) =
      PlayerChat (Controllers.Game.Api.ChatMessage displayName chatMessage sentTime)

subscribeGameMessages :: TChan GameMessage -> ConduitT () GameMessage IO ()
subscribeGameMessages = chanSource

handleBroadcastMessages :: C.Connection -> TChan GameMessage -> CR.Chatroom -> Maybe UTCTime -> IO ()
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
        response <- liftIO $ performRequest serverGame chatroom (definitionService app) (definitionRepository app) (appConnPool app) maybeUser parsedCommand
        C.sendTextData connection $ toJSONResponse $ response

sendInitialGameState :: C.Connection -> ServerGameSnapshot -> Maybe AuthUser -> IO ()
sendInitialGameState connection serverGameSnapshot maybeUser = do
  let initialGameState = initialSocketMessage serverGameSnapshot maybeUser
  mapM_ (C.sendTextData connection . toJSONResponse) initialGameState

handleWebsocket :: App -> C.Connection -> Text -> Maybe AuthUser -> Maybe UTCTime -> IO ()
handleWebsocket app connection gameId maybeUser chatMessagesSince = runResourceT $ do
  (_, eitherServerGame) <- getCacheableResource (games app) gameId
  (_, eitherChatRoom) <- getCacheableResource (chatRooms app) gameId

  case (eitherServerGame, eitherChatRoom) of
    (Left err, _) -> liftIO $ C.sendTextData connection (toJSONResponse (InvalidCommand err))
    (_, Left err) -> liftIO $ C.sendTextData connection (toJSONResponse (InvalidCommand err))
    (Right serverGame, Right chatroom) -> do
      let inactivityTrackerState = inactivityTracker app
      (channel, gameSnapshot) <- liftIO (atomically $ (,) <$> dupTChan (broadcastChannel serverGame) <*> makeServerGameSnapshot serverGame)
      liftIO $ withTrackWebsocketActivity inactivityTrackerState $ do
        withNotifyJoinAndLeave (appConnPool app) serverGame maybeUser $ do
          sendInitialGameState connection gameSnapshot maybeUser
          let handleOutbound = handleBroadcastMessages connection channel chatroom chatMessagesSince
          let handleInbound = handleInboundSocketMessages app connection chatroom serverGame maybeUser
          race_ handleOutbound handleInbound

gameApp :: App -> Text -> Maybe AuthUser -> WebSocketsT Handler ()
gameApp app gameId maybeUser = do
  connection <- ask
  chatMessagesSinceParam <- lift chatMessageSinceQueryParamValue
  liftIO (handleWebsocket app connection gameId maybeUser chatMessagesSinceParam)

gameToMoveSummaries :: G.Game -> Either Text [MoveSummary]
gameToMoveSummaries game =
  if not (null moves)
    then do
      let gameTransitions = restoreGameLazy emptyGame $ NE.fromList (toList moves)
      let reconstructedGameSummaries = sequence . map (fmap transitionToSummary) $ NE.toList gameTransitions
      case reconstructedGameSummaries of
        Left err -> Left (pack (show err))
        Right summaries -> Right summaries
    else return []
  where
    -- TODO: Add function to make a new empty game from a game history to
    -- haskellscrabble
    Right playersState = makeGameStatePlayers (L.length $ G.players game)
    Right emptyGame = G.makeGame playersState originalBag (G.dictionary game)
    (G.History originalBag moves) = G.history game

{-
    Send the chat log history to the client from the given date onwards if supplied (otherwise from the beginning)
-}
sendPreviousChatMessages :: Pool SqlBackend -> Text -> Maybe UTCTime -> C.Connection -> IO ()
sendPreviousChatMessages pool gameId Nothing connection = do
  liftIO
    $ flip runSqlPersistMPool pool
    $ getChatMessages gameId
    $$ CL.map toJSONResponse $= CL.mapM_ (liftIO . C.sendTextData connection)
sendPreviousChatMessages pool gameId (Just since) connection = do
  liftIO
    $ flip runSqlPersistMPool pool
    $ getChatMessagesSince gameId since
    $$ CL.map toJSONResponse $= CL.mapM_ (liftIO . C.sendTextData connection)

sendPreviousDefinitions :: DefinitionRepositoryImpl -> Text -> Maybe UTCTime -> C.Connection -> IO ()
sendPreviousDefinitions definitionRepository gameId since connection =
  runConduit $ getGameDefinitionsImpl definitionRepository gameId .| CL.filter (isAfter since) .| CL.map mapDefinitions .| CL.map toJSONResponse .| CL.mapM_ (liftIO . C.sendTextData connection)
  where
    mapDefinitions :: GameWordItem -> GameMessage
    mapDefinitions (GameWordItem word createdAt definitions) =
      let wordDefinitions = map makeDefinition definitions
       in WordDefinitions word createdAt wordDefinitions

    isAfter :: Maybe UTCTime -> GameWordItem -> Bool
    isAfter Nothing (GameWordItem _ createdAt _) = True
    isAfter (Just since) (GameWordItem _ createdAt _) = createdAt > since

    makeDefinition :: WordDefinitionItem -> D.Definition
    makeDefinition (WordDefinitionItem partOfSpeech definition example) =
      D.Definition partOfSpeech definition example