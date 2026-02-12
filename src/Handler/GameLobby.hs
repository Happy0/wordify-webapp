{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Handler.GameLobby where

import Control.Applicative
import Control.Concurrent
import qualified Control.Monad as CM
import Control.Monad.Loops
import Controllers.Common.CacheableSharedResource (getCacheableResource, withCacheableResource)
import Controllers.Game.Api
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.GameLobby.Api
import Controllers.GameLobby.CreateGame
import Controllers.GameLobby.GameLobby
import Controllers.GameLobby.Model.GameLobby
import qualified Controllers.GameLobby.Model.GameLobby as GL
import Controllers.User.Model.AuthUser (AuthUser (AuthUser, nickname))
import Controllers.User.Persist (getUser)
import Data.Aeson
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Foundation
import Import
import InactivityTracker
import Model.Api
import Network.Mail.Mime
import qualified Network.WebSockets.Connection as C
import System.Random
import Web.Cookie
import Yesod.Core
import Yesod.WebSockets

getCreateGamePageR :: Handler Html
getCreateGamePageR = do
    app <- getYesod
    liftIO $ trackRequestReceivedActivity (inactivityTracker app)
    maid <- maybeAuthId
    case maid of
      Nothing -> gamePagelayout $ renderNotLoggedInLobbyPage "Login / Sign Up to Join the Lobby"
      Just _ -> gamePagelayout $ do
        addStylesheet $ (StaticR css_wordify_css)
        addScript $ StaticR wordifyJs
        [whamlet|
          <div #createlobby>
              
            |]
        toWidget
          [julius|
            
            const lobby = Wordify.createCreateGame('#createlobby', {
              locales: {
                "English": "en",
                "Spanish (FISE)": "es_fise"
              },
              isLoggedIn: true
            });
          |]

{-
  Creates a new game lobby for inviting players to a game via a link.

  Returns the gameId of the new game lobby for the client to use to join
  the new lobby.
-}
postCreateGameR :: Handler Text
postCreateGameR =
  do
    app <- getYesod
    liftIO $ trackRequestReceivedActivity (inactivityTracker app)
    CreateGameLobby numPlayers locale <- requireJsonBody :: Handler CreateGameLobby
    gameCreateResult <- liftIO (createGame app numPlayers locale)
    case gameCreateResult of
      Left err -> invalidArgs [err]
      Right gameId -> return gameId

renderNotLoggedInLobbyPage :: Text -> WidgetFor App ()
renderNotLoggedInLobbyPage message = do
  addStylesheet $ (StaticR css_wordify_css)
  addScript $ StaticR wordifyJs
  [whamlet|
    <div #lobbylogin>
        
      |]
  toWidget
    [julius|
      var url = document.URL;
      var webSocketUrl = url.replace("http:", "ws:").replace("https:", "wss:");
      
      const login = Wordify.createLogin('#lobbylogin', {
        message: #{message}
      });
    |]


{-
  Joins an existing lobby.

  If the game is in progress, redirects to the game. If an error occurs,
  returns an error page.
-}
getGameLobbyR :: Text -> Handler Html
getGameLobbyR gameId =
  do
    app <- getYesod
    liftIO $ trackRequestReceivedActivity (inactivityTracker app)
    maid <- maybeAuthId
    case maid of
      Just userId -> handlerLobbyAuthenticated gameId userId
      Nothing -> gamePagelayout $ renderNotLoggedInLobbyPage "Login / Sign Up to Join the Lobby"

handlerLobbyAuthenticated :: Text -> Text -> Handler Html
handlerLobbyAuthenticated gameId userId =
  do
    app <- getYesod
    let playerId = userId
    webSockets $ lobbyWebSocketHandler app gameId playerId

    runResourceT $ do
      (_, lobby) <- getCacheableResource (gameLobbies app) gameId
      case lobby of
        Left _ -> lift $ redirectHandler gameId
        Right gameLobby -> do
          join <- liftIO $ joinClient app gameLobby gameId userId
          lift $ renderLobbyPage join gameId

redirectHandler :: Text -> Handler Html
redirectHandler gameId = redirect (GameR gameId)

renderLobbyPage :: Either LobbyInputError GL.ClientLobbyJoinResult -> Text -> Handler Html
renderLobbyPage (Left InvalidPlayerID) gameId = invalidArgs ["Invalid player ID given by browser"]
renderLobbyPage (Left _) gameId = redirectHandler gameId
renderLobbyPage (Right (GL.ClientLobbyJoinResult broadcastChannel (Just gameCreated) _ _)) gameId = redirectHandler gameId
renderLobbyPage (Right (GL.ClientLobbyJoinResult broadcastChannel _ _ lobbySnapshot)) gameId = gamePagelayout $ do
  let joinedPlayerNames = map name (snapshotLobbyPlayers lobbySnapshot)

  addStylesheet $ (StaticR css_wordify_css)
  addScript $ StaticR wordifyJs
  [whamlet|
    <div #lobby>
        
      |]
  toWidget
    [julius|
      var url = document.URL;
      var webSocketUrl = url.replace("http:", "ws:").replace("https:", "wss:");
      
      const lobby = Wordify.createGameLobby('#lobby', {
        playerCount: #{toJSON (snapshotAwaiting lobbySnapshot)},
        gameLobbyId: #{toJSON gameId},
        joinedPlayers: #{toJSON joinedPlayerNames},
        language: #{toJSON (snapShotgameLanguage lobbySnapshot) },
        websocketUrl: webSocketUrl,
        isLoggedIn: true
      });
    |]

lobbyWebSocketHandler :: App -> T.Text -> T.Text -> WebSocketsT Handler ()
lobbyWebSocketHandler app gameId playerId = do
  {-
      We race a thread that sends pings to the client so that when the client closes its connection,
      our loop which reads from the message channel is closed due to the thread being cancelled.
  -}
  connection <- ask
  liftIO $
    withTrackWebsocketActivity (inactivityTracker app) $
      withCacheableResource (gameLobbies app) gameId $ \lobby ->
        case lobby of
          Left err -> putStrLn err >> return ()
          Right lobby -> do
            lobbyChannel <- atomically $ dupTChan (channel lobby)
            race_
              (forever $ atomically (toJSONResponse . handleChannelMessage <$> readTChan lobbyChannel) >>= C.sendTextData connection)
              (forever $ C.sendPing connection ("hello~" :: Text) >> threadDelay 60000000)

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