module Handler.Home.Home where

import qualified Data.Map as M
import ClassyPrelude.Yesod
import qualified Data.Text as T
import Foundation
import Repository.GameRepository
import Yesod.Auth
import Import.NoFoundation (wordifyCss, wordifyHomeJs)
import Model.GameSetup (LocalisedGameSetup(..), TileValues)
import ClassyPrelude (undefined, Maybe (Nothing))
import Controllers.User.Model.AuthUser (AuthUser(AuthUser), ident)
import Yesod.WebSockets
import Network.WebSockets (Connection, sendTextData)
import Controllers.Game.Model.UserEventSubscription (UserEvent (..))
import Modules.UserEvent.Api (UserEventService)
import Modules.Games.Api (GameService, peekGame)
import Control.Monad.Loops (iterateM_)
import qualified Network.WebSockets.Connection as C
import Data.Aeson (encode, eitherDecode)
import Controllers.Game.Model.ServerGame (ServerGameSnapshot(..), ServerGame, lastMove, playing, makeServerGameSnapshot, currentPlayerToMove)
import qualified Controllers.Game.Model.ServerPlayer as SP
import qualified Controllers.User.Model.ServerUser as SU
import Wordify.Rules.Board (textRepresentation)
import Wordify.Rules.Game (board)
import Handler.Common (defaultPageLayout)
import Handler.Common.ClientNotificationPresentation (notificationsForUser, sendNotificationUpdate, notificationsWebSocketHandler)
import Modules.Chats.Api (ChatService, getChatroom, subscribeMessagesLive, getMessagesSinceTime)
import Data.Time.Clock (addUTCTime)
import qualified Modules.Chats.Api as CR (ChatMessage (ChatMessage), Chatroom, sendMessage, SendMessage (SendMessage))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL (map, mapM_)
import qualified Handler.Common.Chat as HC
import Handler.Common.Chat (sendChatUpdate)
import Handler.Home.Model.Outbound
import Handler.Home.Model.Inbound
import Modules.TV.Api (TvService, currentHomeTVState, subscribeHomeTV)
import Modules.TV.Home (HomeTvUpdate(..))

mapGameSummaryEntity :: GameSummaryEntity -> [Text] -> SU.ServerUser -> ActiveGameSummary
mapGameSummaryEntity (GameSummaryEntity gameId latestActivity myMove boardString localisedGameSetup allPlayers) activePlayerNames currentUser =
  let tileValues = tileLettersToValueMap localisedGameSetup
      otherPlayers' = filter ((/= SU.userId currentUser) . SU.userId) allPlayers
      resolvedNames = zipWith resolvePlayerName [1..] otherPlayers'
      otherPlayersWithStatus = map (\name -> OtherPlayer name (name `elem` activePlayerNames)) resolvedNames
  in ActiveGameSummary gameId boardString myMove latestActivity tileValues otherPlayersWithStatus
  where
    resolvePlayerName :: Int -> SU.ServerUser -> Text
    resolvePlayerName n u = fromMaybe (T.pack ("Player " ++ show n)) (SU.username u)

buildActiveGameSummary :: GameSummaryEntity -> Maybe ServerGame -> SU.ServerUser -> IO ActiveGameSummary
buildActiveGameSummary gameSummary Nothing serverUser = pure $ mapGameSummaryEntity gameSummary [] serverUser
buildActiveGameSummary gameSummary (Just serverGame) serverUser = do
  players <- mapM (readTVarIO . snd) (playing serverGame)
  let activeNames = [ defaultPlayerName i p | (i, p) <- zip [1..] players, SP.numConnections p > 0 ]
  pure $ mapGameSummaryEntity gameSummary activeNames serverUser

snapshotToTvSummary :: ServerGameSnapshot -> TvActiveGameSummary
snapshotToTvSummary snapshot =
  let players = zipWith (\i p -> OtherPlayer (defaultPlayerName i p) (SP.numConnections p > 0))
                  [1..] (snapshotPlayers snapshot)
  in TvActiveGameSummary
      (snapshotGameId snapshot)
      (T.pack (textRepresentation (board (gameState snapshot))))
      (lastMove snapshot)
      (tileLettersToValueMap (gameLocalisation snapshot))
      players

renderNotLoggedInPage :: Handler Html
renderNotLoggedInPage = do
  app <- getYesod
  maybeTvGame <- liftIO $ currentHomeTVState (tvService app)
  let tvGameJson = toJSON (fmap snapshotToTvSummary maybeTvGame)
  defaultPageLayout $ do
    addStylesheet $ StaticR wordifyCss
    toWidgetHead [hamlet|<script type="module" src="@{StaticR wordifyHomeJs}">|]
    [whamlet|
      <div #home>

        |]
    toWidget
      [julius|
        window.addEventListener('DOMContentLoaded', function() {
          Wordify.createHome('#home', {
            isLoggedIn: false,
            games: [],
            tileValues: {},
            initialHomeTvGame: #{tvGameJson}
          });
        });
      |]

renderPlayerMoveNote :: Bool -> Widget
renderPlayerMoveNote False = [whamlet| <span> |]
renderPlayerMoveNote True = [whamlet| <span> (Your move) |]

renderActiveGamePage :: (GameRepository a) => App -> a -> SU.ServerUser -> Handler Html
renderActiveGamePage app gameRepository serverUser = do
  activeGames <- liftIO $ getActiveUserGames gameRepository (SU.userId serverUser)
  summaries <- liftIO $ buildActiveGameSummaries (gameService app) activeGames serverUser
  notifs <- liftIO $ notificationsForUser app (SU.userId serverUser)
  maybeTvGame <- liftIO $ currentHomeTVState (tvService app)
  let tvGameJson = toJSON (fmap snapshotToTvSummary maybeTvGame)
  defaultPageLayout $ do
    addStylesheet (StaticR wordifyCss)
    toWidgetHead [hamlet|<script type="module" src="@{StaticR wordifyHomeJs}">|]
    [whamlet|
      <div #home>

        |]
    toWidget
      [julius|
        window.addEventListener('DOMContentLoaded', function() {
          Wordify.createHome('#home', {
            isLoggedIn: true,
            games: #{toJSON summaries},
            tileValues: {},
            notifications: #{toJSON notifs},
            initialHomeTvGame: #{tvGameJson}
          });
        });
      |]

getHomeR :: Handler Html
getHomeR = do
  app <- getYesod
  maybePlayerId <- maybeAuthId

  case maybePlayerId of
    Nothing -> renderNotLoggedInPage
    Just _ -> do
      authedUser <- requireUsername
      let serverUser = authenticatedServerUser authedUser
      {- If this is a websocket request the handler short circuits here, otherwise it goes on to return the HTML page -}
      webSockets $ homeWebsocketHandler app serverUser
      renderActiveGamePage app (gameRepository app) serverUser


data HomeOutboundMessage
  = HomeUserEventMsg UserEvent
  | HomeChatMessage HC.ChatMessage
  | HomeTvMsg HomeTvUpdate

toChatMessage :: CR.ChatMessage -> HC.ChatMessage
toChatMessage (CR.ChatMessage _ displayName msg sentTime messageNumber) =
  HC.ChatMessage displayName msg sentTime messageNumber

homeWebsocketHandler :: App -> SU.ServerUser -> WebSocketsT Handler ()
homeWebsocketHandler app serverUser =
  notificationsWebSocketHandler app (SU.userId serverUser) $ \userEventChan -> do
    connection <- ask
    liftIO $ runResourceT $ do
      chatroomResult <- getChatroom (chatService app) "Home"
      case chatroomResult of
        Right chatroom -> liftIO $ do
          liveChatChan <- subscribeMessagesLive chatroom
          tvChan <- subscribeHomeTV (tvService app)
          let handleOutbound = handleOutboundHomeWebsocket (gameRepository app) (gameService app) (tvService app) connection serverUser chatroom userEventChan liveChatChan tvChan
              handleInbound  = handleInboundHomeWebsocket connection chatroom serverUser
          race_ handleOutbound handleInbound
        _ -> return ()

handleOutboundHomeWebsocket :: (GameRepository a) => a -> GameService -> TvService -> C.Connection -> SU.ServerUser -> CR.Chatroom -> TChan UserEvent -> TChan CR.ChatMessage -> TChan HomeTvUpdate -> IO ()
handleOutboundHomeWebsocket gameRepository gamesCache tvSvc connection serverUser chatroom userEventChan liveChatChan tvChan = do
  activeGames <- getActiveUserGames gameRepository (SU.userId serverUser)
  activeSummaryMap <- buildActiveGameSummaryMap gamesCache activeGames serverUser
  sendGameSummaryEntityState connection activeSummaryMap
  now <- getCurrentTime
  let twoMonthsAgo = addUTCTime (negate $ 60 * 60 * 24 * 61) now
  sendPreviousHomeChatMessages connection chatroom twoMonthsAgo
  maybeTvState <- currentHomeTVState tvSvc
  mapM_ (C.sendTextData connection . encode . TvUpdate . snapshotToTvSummary) maybeTvState
  flip iterateM_ activeSummaryMap $ \currentState -> do
    let nextUserEvent = HomeUserEventMsg <$> readTChan userEventChan
        nextChatMsg   = HomeChatMessage . toChatMessage <$> readTChan liveChatChan
        nextTvMsg     = HomeTvMsg <$> readTChan tvChan
    nextMessage <- atomically (nextUserEvent `orElse` nextChatMsg `orElse` nextTvMsg)
    handleHomeOutboundMessage serverUser connection nextMessage currentState

handleInboundHomeWebsocket :: C.Connection -> CR.Chatroom -> SU.ServerUser -> IO ()
handleInboundHomeWebsocket connection chatroom serverUser = forever $ do
  msg <- C.receiveData connection
  case eitherDecode msg of
    Left _err -> return ()
    Right (SendChatMessage message) ->
      CR.sendMessage chatroom (CR.SendMessage (SU.userId serverUser) (fromMaybe "" (SU.username serverUser)) message)

handleHomeOutboundMessage :: SU.ServerUser -> C.Connection -> HomeOutboundMessage -> Map Text ActiveGameSummary -> IO (Map Text ActiveGameSummary)
handleHomeOutboundMessage serverUser connection (HomeUserEventMsg event) state =
  handleUserEvent serverUser connection event state
handleHomeOutboundMessage _ connection (HomeChatMessage chatMsg) state = do
  sendChatUpdate connection chatMsg
  pure state
handleHomeOutboundMessage _ connection (HomeTvMsg (HomeTVUpdate snapshot)) state = do
  C.sendTextData connection (encode (TvUpdate (snapshotToTvSummary snapshot)))
  pure state

handleUserEvent :: SU.ServerUser -> C.Connection -> UserEvent -> Map Text ActiveGameSummary -> IO (Map Text ActiveGameSummary)
handleUserEvent serverUser connection (MoveInUserGame gameId serverGameSnapshot) state = do
  let userToMove = isUserToMove (SU.userId serverUser) serverGameSnapshot
  let gameSummary = gameSummaryFromServerGame serverGameSnapshot userToMove
  let activeSummary = mapGameSummaryEntity gameSummary (activePlayerNamesFromSnapshot serverGameSnapshot) serverUser
  let newState = M.insert gameId activeSummary state
  sendGameSummaryEntityState connection newState
  pure newState
handleUserEvent _ connection (GameOver gameId _) state = do
  let newState = M.delete gameId state
  sendGameSummaryEntityState connection newState
  pure newState
handleUserEvent serverUser connection (NewGame gameId serverGameSnapshot) state = do

  let userToMove = isUserToMove (SU.userId serverUser) serverGameSnapshot
  let gameSummary = gameSummaryFromServerGame serverGameSnapshot userToMove
  let activeSummary = mapGameSummaryEntity gameSummary (activePlayerNamesFromSnapshot serverGameSnapshot) serverUser
  let newState = M.insert gameId activeSummary state
  sendGameSummaryEntityState connection newState
  pure newState
handleUserEvent _ connection (PlayerActivityChanged gId activeNames) state = do
  let newState = M.adjust (updateActivePlayers activeNames) gId state
  sendGameSummaryEntityState connection newState
  pure newState
handleUserEvent _ connection (NotificationsChanged notifUpdate) state = do
  sendNotificationUpdate connection notifUpdate
  pure state

isUserToMove :: T.Text -> ServerGameSnapshot -> Bool
isUserToMove ident snapshot = currentPlayerToMove snapshot == Just ident

updateActivePlayers :: [Text] -> ActiveGameSummary -> ActiveGameSummary
updateActivePlayers activeNames summary =
  summary { otherPlayers = map (\p -> p { playerActive = playerName p `elem` activeNames }) (otherPlayers summary) }

sendGameSummaryEntityState :: C.Connection -> Map Text ActiveGameSummary -> IO ()
sendGameSummaryEntityState connection state =
  C.sendTextData connection (encode (GamesUpdate (M.elems state)))

sendPreviousHomeChatMessages :: C.Connection -> CR.Chatroom -> UTCTime -> IO ()
sendPreviousHomeChatMessages connection chatroom since =
  runConduit $ getMessagesSinceTime chatroom since
    .| CL.map toChatMessage
    .| CL.mapM_ (sendChatUpdate connection)

gameSummaryFromServerGame :: ServerGameSnapshot -> Bool -> GameSummaryEntity
gameSummaryFromServerGame serverGameSnapshot userToMove =
  GameSummaryEntity
    (snapshotGameId serverGameSnapshot)
    (Just (lastMove serverGameSnapshot))
    userToMove
    (T.pack (textRepresentation (board (gameState serverGameSnapshot))))
    (gameLocalisation serverGameSnapshot)
    (map SP.user (snapshotPlayers serverGameSnapshot))

activePlayerNamesFromSnapshot :: ServerGameSnapshot -> [Text]
activePlayerNamesFromSnapshot snapshot =
  [ defaultPlayerName i p | (i, p) <- zip [1..] (snapshotPlayers snapshot), SP.numConnections p > 0 ]

buildActiveGameSummaries :: GameService -> [GameSummaryEntity] -> SU.ServerUser -> IO [ActiveGameSummary]
buildActiveGameSummaries gamesCache gameSummaries serverUser =
  forM gameSummaries $ \g -> runResourceT $ do
    (_, maybeServerGame) <- peekGame gamesCache (gameSummaryGameId g)
    liftIO $ buildActiveGameSummary g maybeServerGame serverUser

buildActiveGameSummaryMap :: GameService -> [GameSummaryEntity] -> SU.ServerUser -> IO (Map Text ActiveGameSummary)
buildActiveGameSummaryMap gamesCache gameSummaries serverUser = do
  summaries <- buildActiveGameSummaries gamesCache gameSummaries serverUser
  let gameIds = map gameSummaryGameId gameSummaries
  return $ M.fromList (zip gameIds summaries)

defaultPlayerName :: Int -> SP.ServerPlayer -> Text
defaultPlayerName n player = fromMaybe (T.pack ("Player " ++ show n)) (SP.playerUsername player)
