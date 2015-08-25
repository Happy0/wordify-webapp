module Handler.GameLobby where

    import Import
    import Yesod.Core
    import Yesod.WebSockets
    import qualified Data.Map as M
    import Controllers.Game.Model.GameLobby

    getGameLobbyR :: Text -> Handler Html
    getGameLobbyR gameId = defaultLayout $ do
        app <- getYesod
        let lobbies = gameLobbies app

        gameLobby <- liftIO $ getGameLobby gameId lobbies

        [whamlet|
            <div .lobby>

        |]
        toWidget
            [julius|

                var url = document.URL;
                url = url.replace("http:", "ws:").replace("https:", "wss:");
                var conn = new WebSocket(url);

                conn.onmessage = function(e) {
                    alert("New connection~~");
                };

            |]

    getGameLobby :: Text -> TVar (Map Text (TVar GameLobby)) -> IO (Maybe (TVar GameLobby))
    getGameLobby gameId lobbies = atomically $ do
                lobby <- readTVar lobbies
                return $ M.lookup gameId lobby

    lobbyWebSocketHandler :: WebSocketsT Handler ()
    lobbyWebSocketHandler = undefined