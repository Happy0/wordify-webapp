module Handler.GameLobby where

    import Import
    import Foundation
    import Yesod.Core
    import Yesod.WebSockets
    import qualified Data.Map as M
    import Controllers.Game.Model.ServerPlayer
    import Controllers.Game.Model.ServerGame
    import Model.Api
    import Controllers.GameLobby.Model.GameLobby
    import Controllers.GameLobby.GameLobby
    import qualified Data.Text as T
    import Controllers.GameLobby.Api
    import Data.Aeson
    import Model.Api
    import qualified Control.Monad as CM
    import Control.Applicative
    import Control.Concurrent
    import Control.Monad.Loops
    import Controllers.Game.Api
    import Controllers.GameLobby.Api
    import Controllers.GameLobby.CreateGame
    import Data.Either
    import Web.Cookie
    import qualified Data.Text.Encoding as E
    import System.Random
    import Network.Mail.Mime
    import Controllers.User.Persist(getUser)
    import Controllers.User.Model.AuthUser(AuthUser(AuthUser))

    {-
      Creates a new game lobby for inviting players to a game via a link.

      Returns the gameId of the new game lobby for the client to use to join
      the new lobby.
    -}
    postCreateGameR ::  Handler Text
    postCreateGameR =
      do
        CreateGameLobby numPlayers locale <- requireJsonBody :: Handler CreateGameLobby
        app <- getYesod
        gameCreateResult <- liftIO (createGame app numPlayers locale)
        case gameCreateResult of
          Left err -> invalidArgs [err]
          Right gameId -> return gameId

    playerFromDatabase :: App -> Text -> Handler ServerPlayer
    playerFromDatabase app playerId = do
        let pool = appConnPool app
        user <- liftIO $ getUser pool playerId

        case user of 
            Just (AuthUser ident realName nickname) ->
                let playerName = nickname <|> realName
                in return $ makeNewPlayer playerName playerId
            Nothing -> return $ makeNewPlayer Nothing playerId

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
            maid <- maybeAuthId
            case maid of
                Just userId -> handlerLobbyAuthenticated gameId userId
                Nothing -> defaultLayout $ renderNotLoggedInLobbyPage gameId


    handlerLobbyAuthenticated :: Text -> Text -> Handler Html
    handlerLobbyAuthenticated gameId userId =
        do
            app <- getYesod
            player <- playerFromDatabase app userId
            let playerId = identifier player

            initialisationResult <- liftIO $ joinClient app gameId player
            case initialisationResult of
              Left GameLobbyDoesNotExist -> redirect (GameR gameId)
              Left InvalidPlayerID -> invalidArgs ["Invalid player ID given by browser"]
              Right joinResult@(ClientLobbyJoinResult broadcastChannel _) -> do
                if (gameStarted joinResult) then
                    -- If the game was created, the last connected client wouldn't get the 'Game Started'
                    -- event over the websocket because this second request for the websocket connection would no
                    -- longer see the game in the lobby list resulting in a HTTP redirect given to the websocket
                    -- which it cannot handle
                    redirect (GameR gameId)
                else
                    do
                        webSockets $ lobbyWebSocketHandler broadcastChannel gameId playerId
                        defaultLayout renderLobbyPage

    renderLobbyPage = do
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

    lobbyWebSocketHandler :: TChan LobbyMessage -> T.Text -> T.Text -> WebSocketsT Handler ()
    lobbyWebSocketHandler channel gameId playerId = do
        {-
            We race a thread that sends pings to the client so that when the client closes its connection,
            our loop which reads from the message channel is closed due to the thread being cancelled.
        -}
        race_
            (forever $ (atomically $ toJSONResponse . handleChannelMessage <$> readTChan channel) >>= sendTextData)
            (whileM_ (isRight <$> sendPingE ("hello~" :: Text)) $ liftIO $ threadDelay 60000000)
