module Handler.GameLobby where

    import Import
    import Yesod.Core
    import Yesod.WebSockets
    import qualified Data.Map as M
    import Controllers.Game.Model.GameLobby
    import Controllers.Game.Model.ServerPlayer
    import Model.Api
    import Controllers.GameLobby.GameLobby
    import qualified Data.Text as T
    import Controllers.GameLobby.Api
    import Data.Aeson
    import Model.Api

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

    lobbyWebSocketHandler :: TVar GameLobby -> ServerPlayer -> WebSocketsT Handler ()
    lobbyWebSocketHandler gameLobby player =
        do
            comChannel <- liftIO $ atomically $ readTVar gameLobby >>= (cloneTChan . channel)
            race_
                -- Read from the communication channel and update the client of any new events
                (forever $ (atomically $ toJSONResponse . handleChannelMessage <$> readTChan comChannel) >>= sendTextData )

                -- Read from the client's websocket connection
                (forever $
                        -- TODO: Abstract this in some way... It's pretty much a repeat of the home websocket handler
                        do
                            requestText <- receiveData
                            let request = eitherDecode requestText :: Either String ClientRequest
                            case request of
                                Right requestData -> 
                                    do
                                        result <- liftIO $ handleClientMessage gameLobby requestData
                                        case result of
                                                Left clientError -> 
                                                    sendTextData $ toJSONResponse $ ClientError clientError
                                                Right requestResponse ->
                                                    sendTextData $ toJSONResponse requestResponse
                                Left reason -> 
                                    sendTextData $ toJSONResponse (ClientError $ T.pack reason)

                )

