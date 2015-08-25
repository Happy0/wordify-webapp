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

        case gameLobby of
            Nothing ->
                {- Perhaps the game has already started... Redirect the user
                   to the game URL as a spectator. If the redirect fails,
                   the user will be shown the appropriate error.
                -}
                redirect $ GameR gameId
            Just gameLobby ->
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
    getGameLobby gameId lobbies = atomically $ M.lookup gameId <$> readTVar lobbies

    lobbyWebSocketHandler :: TVar GameLobby -> WebSocketsT Handler ()
    lobbyWebSocketHandler gameLobby = undefined