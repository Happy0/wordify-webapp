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

renderNotLoggedInLobbyPage gameId = do
  addStylesheet $ (StaticR css_lobby_css)
  [whamlet|
            <p> You must log in to join this game
            <a href=@{AuthR LoginR}>Login
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
      Nothing -> defaultLayout $ renderNotLoggedInLobbyPage gameId

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
renderLobbyPage (Right (GL.ClientLobbyJoinResult broadcastChannel (Just gameCreated) _)) gameId = redirectHandler gameId
renderLobbyPage (Right (GL.ClientLobbyJoinResult broadcastChannel _ _)) gameId = defaultLayout $ do
  addStylesheet $ (StaticR css_lobby_css)
  [whamlet|
          <div #lobby>
              <div #url-box>
                  <p .url-box-text> To invite players to the game, tell them to visit the following URL:
                  <div>
                      <span>
                          <input .url-box-text #lobby-url readonly='true'>
                      <span>
                          <button .copy data-rel="lobby-url" > copy
                  <p .url-box-text> The game will begin once enough players have visited the URL.
      |]
  toWidget
    [julius|
              // Copied from: https://github.com/ornicar/lila/blob/67c1fc62b6c8d3041af15cf25bdf7a38fff5c383/public/javascripts/big.js#L687
              $('#lobby').on('click', 'button.copy', function() {
                  var prev = $('#' + $(this).data('rel'));
                  if (!prev) return;
                  var usePrompt = function() {
                  prompt('Your browser does not support automatic copying. Copy this text manually with Ctrl + C:', prev.val());
                  };
              try {
              if (document.queryCommandSupported('copy')) {
                  // Awesome! Done in five seconds, can go home.
                  prev.select();
                  document.execCommand('copy');
              } else if (window.clipboardData) {
                  // For a certain specific Internet Explorer version *cough cough IE8*
                  window.clipboardData.setData('Text', prev.val());
              } else throw 'nope';
              $(this).attr('data-icon', 'E');
              } catch (e) {
              usePrompt();
              }
          });

              var url = document.URL;
              $('#lobby-url').val(url);
              url = url.replace("http:", "ws:").replace("https:", "wss:");
              var conn = new WebSocket(url);

              conn.onmessage = function(e) {
                  var data = JSON.parse(e.data);
                  parseServerMessage(data);
              };

              var parseServerMessage = function(serverMessage)
              {
                  console.dir(serverMessage);

                  if (serverMessage && serverMessage.command)
                  {

                      if (serverMessage.command === "startGame")
                      {
                          handleGameStarted(serverMessage.payload.gameId);
                      }
                  }
              };

              var handleGameStarted = function(gameId)
              {
                  console.info("Game started");
                  window.location = "/games" + "/" + gameId;
              };

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
