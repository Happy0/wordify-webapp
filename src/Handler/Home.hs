module Handler.Home where

import qualified Data.Map as M
import ClassyPrelude.Yesod
import qualified Data.Text as T
import Foundation
import Repository.GameRepository
import Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend))
import Yesod.Auth
import Import.NoFoundation (js_wordify_js, css_wordify_css)
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
import Controllers.Game.Model.ServerGame (ServerGameSnapshot(..), lastMove)
import qualified Controllers.Game.Model.ServerPlayer as SP
import Wordify.Rules.Board (textRepresentation)
import Wordify.Rules.Game (board)

data ActiveGameSummary = ActiveGameSummary {gameId :: Text, boardString:: Text,yourMove :: Bool, lastActivity :: Maybe UTCTime, tileValues :: TileValues, otherPlayers :: [Text]}

instance ToJSON ActiveGameSummary where
  toJSON (ActiveGameSummary gameId boardString yourMove lastActivity tileValues otherPlayers) = object [
    "gameId" .= gameId,
    "boardString" .= boardString,
    "yourMove" .= yourMove,
    "lastActivity" .= lastActivity,
    "tileValues" .= tileValues,
    "otherPlayers" .= otherPlayers
     ]

mapGameSummary :: GameSummary -> ActiveGameSummary
mapGameSummary summary@(GameSummary gameId latestActivity myMove boardString localisedGameSetup otherPlayerNames) =
  let tileValues = tileLettersToValueMap localisedGameSetup
  in ActiveGameSummary gameId boardString myMove latestActivity tileValues otherPlayerNames

renderNotLoggedInPage :: Handler Html
renderNotLoggedInPage =
  gamePagelayout $ do
    addStylesheet $ (StaticR css_wordify_css)
    addScript $ StaticR js_wordify_js
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
  gamePagelayout $ do
    addStylesheet $ (StaticR css_wordify_css)
    addScript $ StaticR js_wordify_js
    [whamlet|
      <div #home>
          
        |]
    toWidget
      [julius|
        const lobby = Wordify.createHome('#home', {
          isLoggedIn: true,
          games: #{toJSON (map mapGameSummary activeGames)},
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
  liftIO $ handleHomeWebsocket gameRepository connection userIdent userChannels

handleHomeWebsocket :: (GameRepository a) => a -> Connection -> Text -> ResourceCache Text (TChan UserEvent) -> IO ()
handleHomeWebsocket gameRepository connection userIdent userEventBroadcastChannels = runResourceT $ do
    (_, userBroadcastChannel) <- getCacheableResource userEventBroadcastChannels userIdent
    case userBroadcastChannel of
      Left _ -> return ()
      Right channel -> do
        -- Important that we duplicate the channel first so that we don't miss any game updates after fetching from the database
        channelSubscription <- atomically (dupTChan channel)
        activeGames <- liftIO $ getActiveUserGames gameRepository userIdent
        let activeGameMap = M.fromList $ map (\u -> (gameSummaryGameId u, u)) activeGames
        _ <- liftIO (sendGameSummaryState connection activeGameMap)
        flip iterateM_ activeGameMap $ \newActiveGameState -> do
          nextMessage <- atomically (readTChan channelSubscription)
          liftIO (handleUserEvent userIdent connection nextMessage newActiveGameState)

handleUserEvent :: T.Text -> C.Connection -> UserEvent -> Map Text GameSummary -> IO (Map Text GameSummary)
handleUserEvent userIdent connection (MoveInUserGame gameId gameState userToMove) gameStates = do
  let gameSummary = gameSummaryFromServerGame userIdent gameState userToMove
  let newState = M.insert gameId gameSummary gameStates
  sendGameSummaryState connection newState
  pure newState
handleUserEvent userIdent connection (GameOver gameId serverGameSnapshot) gameStates = do
  let newState = M.delete gameId gameStates
  _ <- sendGameSummaryState connection newState
  pure newState
handleUserEvent userIdent connection (NewGame gameId gameState userToMove) gameStates = do
  let gameSummary = gameSummaryFromServerGame userIdent gameState userToMove
  let newState = M.insert gameId gameSummary gameStates
  sendGameSummaryState connection newState
  pure newState

sendGameSummaryState :: C.Connection -> Map Text GameSummary -> IO ()
sendGameSummaryState connection state = do
  let summaries = map snd (M.toList state)
  let x = map mapGameSummary summaries
  let payload = object [ "command" .= ("gamesUpdate" :: T.Text), "payload" .= x ]
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
