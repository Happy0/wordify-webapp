module Handler.Home.Home where

import qualified Data.Map as M
import ClassyPrelude.Yesod
import qualified Data.Text as T
import Foundation
import Repository.GameRepository
import Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend))
import Yesod.Auth
import Import.NoFoundation (wordifyCss, wordifyJs)
import Model.GameSetup (LocalisedGameSetup(..), TileValues)
import ClassyPrelude (undefined, Maybe (Nothing))
import Controllers.User.Model.AuthUser (AuthUser(AuthUser), ident)
import Yesod.WebSockets
import Network.WebSockets (Connection, sendTextData)
import Controllers.Game.Model.UserEventSubscription (UserEvent (..))
import Modules.UserEvent.Api (UserEventService, subscribeToUserChannel)
import Controllers.Common.CacheableSharedResource
import Control.Monad.Loops (iterateM_)
import qualified Network.WebSockets.Connection as C
import Data.Aeson (encode, eitherDecode)
import Controllers.Game.Model.ServerGame (ServerGameSnapshot(..), ServerGame, lastMove, playing, makeServerGameSnapshot, currentPlayerToMove)
import qualified Controllers.Game.Model.ServerPlayer as SP
import Wordify.Rules.Board (textRepresentation)
import Wordify.Rules.Game (board)
import Handler.Common.ClientNotificationPresentation (notificationsForUser, sendNotificationUpdate)
import Modules.Chats.Api (ChatService, getChatroom, subscribeMessagesLive, getMessagesSinceTime)
import Data.Time.Clock (addUTCTime)
import qualified Modules.Chats.Api as CR (ChatMessage (ChatMessage), Chatroom, sendMessage, SendMessage (SendMessage))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL (map, mapM_)
import qualified Handler.Common.Chat as HC
import Handler.Common.Chat (sendChatUpdate)
import Handler.Home.Model.Outbound
import Handler.Home.Model.Inbound

mapGameSummary :: GameSummary -> [Text] -> ActiveGameSummary
mapGameSummary (GameSummary gameId latestActivity myMove boardString localisedGameSetup otherPlayerNames) activePlayerNames =
  let tileValues = tileLettersToValueMap localisedGameSetup
      resolvedNames = zipWith resolvePlayerName [1..] otherPlayerNames
      otherPlayersWithStatus = map (\name -> OtherPlayer name (name `elem` activePlayerNames)) resolvedNames
  in ActiveGameSummary gameId boardString myMove latestActivity tileValues otherPlayersWithStatus
  where
    resolvePlayerName :: Int -> Maybe Text -> Text
    resolvePlayerName n Nothing = T.pack ("Player " ++ show n)
    resolvePlayerName _ (Just name) = name

buildActiveGameSummary :: GameSummary -> Maybe ServerGame -> IO ActiveGameSummary
buildActiveGameSummary gameSummary Nothing = pure $ mapGameSummary gameSummary []
buildActiveGameSummary gameSummary (Just serverGame) = do
  players <- mapM (readTVarIO . snd) (playing serverGame)
  let activeNames = [ defaultPlayerName i p | (i, p) <- zip [1..] players, SP.numConnections p > 0 ]
  pure $ mapGameSummary gameSummary activeNames

renderNotLoggedInPage :: Handler Html
renderNotLoggedInPage =
  gamePagelayout $ do
    addStylesheet $ (StaticR wordifyCss)
    addScript $ StaticR wordifyJs
    [whamlet|
      <div #home>

        |]
    toWidget
      [julius|
        const lobby = Wordify.createHome('#home', {
          isLoggedIn: false,
          games: [],
          tileValues: {}
        });
      |]

renderPlayerMoveNote :: Bool -> Widget
renderPlayerMoveNote False = [whamlet| <span> |]
renderPlayerMoveNote True = [whamlet| <span> (Your move) |]

renderActiveGamePage :: (GameRepository a) => App -> a -> T.Text -> Handler Html
renderActiveGamePage app gameRepository userId = do
  activeGames <- liftIO $ getActiveUserGames gameRepository userId
  summaries <- liftIO $ buildActiveGameSummaries (games app) activeGames
  notifs <- liftIO $ notificationsForUser app userId
  gamePagelayout $ do
    addStylesheet $ (StaticR wordifyCss)
    addScript $ StaticR wordifyJs
    [whamlet|
      <div #home>

        |]
    toWidget
      [julius|
        const lobby = Wordify.createHome('#home', {
          isLoggedIn: true,
          games: #{toJSON summaries},
          tileValues: {},
          notifications: #{toJSON notifs}
        });
      |]

getHomeR :: Handler Html
getHomeR = do
  app <- getYesod
  let pool = appConnPool app
  let gameRepositorySQLBackend = GameRepositorySQLBackend pool (localisedGameSetups app)
  maybePlayerId <- maybeAuthId

  case maybePlayerId of
    Nothing -> renderNotLoggedInPage
    Just _ -> do
      authedUser <- requireUsername
      let userId = authenticatedUserId authedUser
      let displayName = authenticatedUsername authedUser
      {- If this is a websocket request the handler short circuits here, otherwise it goes on to return the HTML page -}
      webSockets $ homeWebsocketHandler app userId displayName
      renderActiveGamePage app gameRepositorySQLBackend userId

-- TODO: don't copypasta this and share it somewhere
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
                    ^{pageHead pc}
                <body>
                    <div .special-wrapper>
                        ^{pageBody pc}
        |]

-- Internal dispatch type for the outbound loop
data HomeOutboundMessage
  = HomeUserEventMsg UserEvent
  | HomeChatMessage HC.ChatMessage

toChatMessage :: CR.ChatMessage -> HC.ChatMessage
toChatMessage (CR.ChatMessage _ displayName msg sentTime messageNumber) =
  HC.ChatMessage displayName msg sentTime messageNumber

-- Acquires resources (chatroom, user event channel) and races the inbound and
-- outbound socket handlers for the duration of the connection.
homeWebsocketHandler :: App -> Text -> Text -> WebSocketsT Handler ()
homeWebsocketHandler app userIdent displayName = do
  connection <- ask
  let gameRepository = GameRepositorySQLBackend (appConnPool app) (localisedGameSetups app)
  liftIO $ runResourceT $ do
    (_, userBroadcastChannelResult) <- subscribeToUserChannel (userEventService app) userIdent
    chatroomResult <- getChatroom (chatService app) "Home"
    case (userBroadcastChannelResult, chatroomResult) of
      (Right userEventChan, Right chatroom) -> liftIO $ do
        liveChatChan <- subscribeMessagesLive chatroom
        let handleOutbound = handleOutboundHomeWebsocket gameRepository (games app) connection userIdent chatroom userEventChan liveChatChan
            handleInbound  = handleInboundHomeWebsocket connection chatroom userIdent displayName
        race_ handleOutbound handleInbound
      _ -> return ()

handleOutboundHomeWebsocket :: (GameRepository a) => a -> ResourceCache Text ServerGame -> C.Connection -> Text -> CR.Chatroom -> TChan UserEvent -> TChan CR.ChatMessage -> IO ()
handleOutboundHomeWebsocket gameRepository gamesCache connection userIdent chatroom userEventChan liveChatChan = do
  activeGames <- getActiveUserGames gameRepository userIdent
  activeSummaryMap <- buildActiveGameSummaryMap gamesCache activeGames
  sendGameSummaryState connection activeSummaryMap
  now <- getCurrentTime
  let twoMonthsAgo = addUTCTime (negate $ 60 * 60 * 24 * 61) now
  sendPreviousHomeChatMessages connection chatroom twoMonthsAgo
  flip iterateM_ activeSummaryMap $ \currentState -> do
    let nextUserEvent = HomeUserEventMsg <$> readTChan userEventChan
        nextChatMsg   = HomeChatMessage . toChatMessage <$> readTChan liveChatChan
    nextMessage <- atomically (nextUserEvent `orElse` nextChatMsg)
    handleHomeOutboundMessage userIdent connection nextMessage currentState

handleInboundHomeWebsocket :: C.Connection -> CR.Chatroom -> Text -> Text -> IO ()
handleInboundHomeWebsocket connection chatroom userIdent displayName = forever $ do
  msg <- C.receiveData connection
  case eitherDecode msg of
    Left _err -> return ()
    Right (SendChatMessage message) ->
      CR.sendMessage chatroom (CR.SendMessage userIdent displayName message)

handleHomeOutboundMessage :: T.Text -> C.Connection -> HomeOutboundMessage -> Map Text ActiveGameSummary -> IO (Map Text ActiveGameSummary)
handleHomeOutboundMessage userIdent connection (HomeUserEventMsg event) state =
  handleUserEvent userIdent connection event state
handleHomeOutboundMessage _ connection (HomeChatMessage chatMsg) state = do
  sendChatUpdate connection chatMsg
  pure state

handleUserEvent :: T.Text -> C.Connection -> UserEvent -> Map Text ActiveGameSummary -> IO (Map Text ActiveGameSummary)
handleUserEvent userIdent connection (MoveInUserGame gameId serverGame) state = do
  snapshot <- atomically (makeServerGameSnapshot serverGame)
  let userToMove = isUserToMove userIdent snapshot
  let gameSummary = gameSummaryFromServerGame userIdent snapshot userToMove
  let activeSummary = mapGameSummary gameSummary (activePlayerNamesFromSnapshot snapshot)
  let newState = M.insert gameId activeSummary state
  sendGameSummaryState connection newState
  pure newState
handleUserEvent _ connection (GameOver gameId _) state = do
  let newState = M.delete gameId state
  sendGameSummaryState connection newState
  pure newState
handleUserEvent userIdent connection (NewGame gameId serverGame) state = do
  snapshot <- atomically (makeServerGameSnapshot serverGame)
  let userToMove = isUserToMove userIdent snapshot
  let gameSummary = gameSummaryFromServerGame userIdent snapshot userToMove
  let activeSummary = mapGameSummary gameSummary (activePlayerNamesFromSnapshot snapshot)
  let newState = M.insert gameId activeSummary state
  sendGameSummaryState connection newState
  pure newState
handleUserEvent _ connection (PlayerActivityChanged gId activeNames) state = do
  let newState = M.adjust (updateActivePlayers activeNames) gId state
  sendGameSummaryState connection newState
  pure newState
handleUserEvent _ connection (NotificationsChanged notifUpdate) state = do
  sendNotificationUpdate connection notifUpdate
  pure state

isUserToMove :: T.Text -> ServerGameSnapshot -> Bool
isUserToMove ident snapshot = currentPlayerToMove snapshot == Just ident

updateActivePlayers :: [Text] -> ActiveGameSummary -> ActiveGameSummary
updateActivePlayers activeNames summary =
  summary { otherPlayers = map (\p -> p { playerActive = playerName p `elem` activeNames }) (otherPlayers summary) }

sendGameSummaryState :: C.Connection -> Map Text ActiveGameSummary -> IO ()
sendGameSummaryState connection state =
  C.sendTextData connection (encode (GamesUpdate (M.elems state)))

sendPreviousHomeChatMessages :: C.Connection -> CR.Chatroom -> UTCTime -> IO ()
sendPreviousHomeChatMessages connection chatroom since =
  runConduit $ getMessagesSinceTime chatroom since
    .| CL.map toChatMessage
    .| CL.mapM_ (sendChatUpdate connection)

gameSummaryFromServerGame :: T.Text -> ServerGameSnapshot -> Bool -> GameSummary
gameSummaryFromServerGame userIdent serverGameSnapshot userToMove =
  let otherPlayers = filter (\p -> SP.playerId p /= userIdent) (snapshotPlayers serverGameSnapshot)
  in GameSummary
    (snapshotGameId serverGameSnapshot)
    (Just (lastMove serverGameSnapshot))
    userToMove
    (T.pack (textRepresentation (board (gameState serverGameSnapshot))))
    (gameLocalisation serverGameSnapshot)
    (map SP.playerUsername otherPlayers)

activePlayerNamesFromSnapshot :: ServerGameSnapshot -> [Text]
activePlayerNamesFromSnapshot snapshot =
  [ defaultPlayerName i p | (i, p) <- zip [1..] (snapshotPlayers snapshot), SP.numConnections p > 0 ]

buildActiveGameSummaries :: ResourceCache Text ServerGame -> [GameSummary] -> IO [ActiveGameSummary]
buildActiveGameSummaries gamesCache gameSummaries =
  forM gameSummaries $ \g -> do
    maybeServerGame <- atomically $ peekCacheableResource gamesCache (gameSummaryGameId g)
    buildActiveGameSummary g maybeServerGame

buildActiveGameSummaryMap :: ResourceCache Text ServerGame -> [GameSummary] -> IO (Map Text ActiveGameSummary)
buildActiveGameSummaryMap gamesCache gameSummaries = do
  summaries <- buildActiveGameSummaries gamesCache gameSummaries
  let gameIds = map gameSummaryGameId gameSummaries
  return $ M.fromList (zip gameIds summaries)

defaultPlayerName :: Int -> SP.ServerPlayer -> Text
defaultPlayerName n player = fromMaybe (T.pack ("Player " ++ show n)) (SP.playerUsername player)
