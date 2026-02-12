module Handler.Home where

import qualified Data.Map as M
import ClassyPrelude.Yesod
import qualified Data.Text as T
import Foundation
import Repository.GameRepository
import Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend))
import Yesod.Auth
import Import.NoFoundation (css_wordify_css, wordifyJs)
import Model.GameSetup (LocalisedGameSetup(..), TileValues)
import ClassyPrelude (undefined, Maybe (Nothing))
import Controllers.User.Model.AuthUser (AuthUser(AuthUser), ident)
import Yesod.WebSockets
import Network.WebSockets (Connection, sendTextData)
import Controllers.Game.Model.UserEventSubscription (UserEvent (..))
import Controllers.Common.CacheableSharedResource
import Control.Monad.Loops (iterateM_)
import qualified Network.WebSockets.Connection as C
import Data.Aeson (encode)
import Controllers.Game.Model.ServerGame (ServerGameSnapshot(..), ServerGame, lastMove, playing, makeServerGameSnapshot, currentPlayerToMove)
import qualified Controllers.Game.Model.ServerPlayer as SP
import Wordify.Rules.Board (textRepresentation)
import Wordify.Rules.Game (board)

data OtherPlayer = OtherPlayer { playerName :: Text, playerActive :: Bool }

instance ToJSON OtherPlayer where
  toJSON (OtherPlayer name active) = object [
    "name" .= name,
    "active" .= active
    ]

data ActiveGameSummary = ActiveGameSummary {gameId :: Text, boardString:: Text,yourMove :: Bool, lastActivity :: Maybe UTCTime, tileValues :: TileValues, otherPlayers :: [OtherPlayer]}

instance ToJSON ActiveGameSummary where
  toJSON (ActiveGameSummary gameId boardString yourMove lastActivity tileValues otherPlayers) = object [
    "gameId" .= gameId,
    "boardString" .= boardString,
    "yourMove" .= yourMove,
    "lastActivity" .= lastActivity,
    "tileValues" .= tileValues,
    "otherPlayers" .= otherPlayers
     ]

mapGameSummary :: GameSummary -> [Text] -> ActiveGameSummary
mapGameSummary (GameSummary gameId latestActivity myMove boardString localisedGameSetup otherPlayerNames) activePlayerNames =
  let tileValues = tileLettersToValueMap localisedGameSetup
      otherPlayersWithStatus = map (\name -> OtherPlayer name (name `elem` activePlayerNames)) otherPlayerNames
  in ActiveGameSummary gameId boardString myMove latestActivity tileValues otherPlayersWithStatus

buildActiveGameSummary :: GameSummary -> Maybe ServerGame -> IO ActiveGameSummary
buildActiveGameSummary gameSummary Nothing = pure $ mapGameSummary gameSummary []
buildActiveGameSummary gameSummary (Just serverGame) = do
  players <- mapM (readTVarIO . snd) (playing serverGame)
  let activeNames = [ fromMaybe "<Unknown>" (SP.name p) | p <- players, SP.numConnections p > 0 ]
  pure $ mapGameSummary gameSummary activeNames

renderNotLoggedInPage :: Handler Html
renderNotLoggedInPage =
  gamePagelayout $ do
    addStylesheet $ (StaticR css_wordify_css)
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
  gamePagelayout $ do
    addStylesheet $ (StaticR css_wordify_css)
    addScript $ StaticR wordifyJs
    [whamlet|
      <div #home>

        |]
    toWidget
      [julius|
        const lobby = Wordify.createHome('#home', {
          isLoggedIn: true,
          games: #{toJSON summaries},
          tileValues: {}
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
    Just userId -> do
      {- If this is a websocket request the handler short circuits here, otherwise it goes on to return the HTML page -}
      webSockets $ homeWebsocketHandler app userId
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

homeWebsocketHandler :: App -> Text -> WebSocketsT Handler ()
homeWebsocketHandler app userIdent = do
  connection <- ask
  let gameRepository = GameRepositorySQLBackend (appConnPool app) (localisedGameSetups app)
  let userChannels = userEventChannels app
  let gamesCache = games app
  liftIO $ handleHomeWebsocket gameRepository gamesCache connection userIdent userChannels

handleHomeWebsocket :: (GameRepository a) => a -> ResourceCache Text ServerGame -> Connection -> Text -> ResourceCache Text (TChan UserEvent) -> IO ()
handleHomeWebsocket gameRepository gamesCache connection userIdent userEventBroadcastChannels = runResourceT $ do
    (_, userBroadcastChannel) <- getCacheableResource userEventBroadcastChannels userIdent
    case userBroadcastChannel of
      Left _ -> return ()
      Right channel -> do
        -- Important that we duplicate the channel first so that we don't miss any game updates after fetching from the database
        channelSubscription <- atomically (dupTChan channel)
        activeGames <- liftIO $ getActiveUserGames gameRepository userIdent
        activeSummaryMap <- liftIO $ buildActiveGameSummaryMap gamesCache activeGames
        _ <- liftIO (sendGameSummaryState connection activeSummaryMap)
        flip iterateM_ activeSummaryMap $ \currentState -> do
          nextMessage <- atomically (readTChan channelSubscription)
          liftIO (handleUserEvent userIdent connection nextMessage currentState)

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

isUserToMove :: T.Text -> ServerGameSnapshot -> Bool
isUserToMove ident snapshot = currentPlayerToMove snapshot == Just ident

updateActivePlayers :: [Text] -> ActiveGameSummary -> ActiveGameSummary
updateActivePlayers activeNames summary =
  summary { otherPlayers = map (\p -> p { playerActive = playerName p `elem` activeNames }) (otherPlayers summary) }

sendGameSummaryState :: C.Connection -> Map Text ActiveGameSummary -> IO ()
sendGameSummaryState connection state = do
  let payload = object [ "command" .= ("gamesUpdate" :: T.Text), "payload" .= M.elems state ]
  C.sendTextData connection (encode payload)

gameSummaryFromServerGame :: T.Text -> ServerGameSnapshot -> Bool -> GameSummary
gameSummaryFromServerGame userIdent serverGameSnapshot userToMove =
  let otherPlayers = filter (\p -> SP.playerId p /= userIdent) (snapshotPlayers serverGameSnapshot)
  in GameSummary
    (snapshotGameId serverGameSnapshot)
    (Just (lastMove serverGameSnapshot))
    userToMove
    (T.pack (textRepresentation (board (gameState serverGameSnapshot))))
    (gameLocalisation serverGameSnapshot)
    (map (fromMaybe "<Unknown>" . SP.name) otherPlayers)

activePlayerNamesFromSnapshot :: ServerGameSnapshot -> [Text]
activePlayerNamesFromSnapshot snapshot =
  [ fromMaybe "<Unknown>" (SP.name p) | p <- snapshotPlayers snapshot, SP.numConnections p > 0 ]

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
