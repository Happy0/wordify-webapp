module Handler.GameLobby where

    import Import
    import Foundation
    import Yesod.Core
    import Yesod.WebSockets
    import qualified Data.Map as M
    import Controllers.Game.Persist
    import Controllers.Game.Model.GameLobby
    import Controllers.Game.Model.ServerPlayer
    import Controllers.Game.Model.ServerGame
    import Model.Api
    import Controllers.GameLobby.GameLobby
    import qualified Data.Text as T
    import Controllers.GameLobby.Api
    import Data.Aeson
    import Model.Api
    import qualified Control.Monad as CM
    import Control.Monad.Trans.Either
    import Control.Error.Util
    import Control.Concurrent
    import Control.Monad.Loops
    import Controllers.Game.Api

    getGameLobbyR :: Text -> Handler Html
    getGameLobbyR gameId =
        do
            app <- getYesod
            maybePlayerId <- lookupCookie "id"

            webSockets $ lobbyWebSocketHandler app gameId maybePlayerId
            defaultLayout $ do
                [whamlet|
                    <div .lobby>
                        <p> To play, send your opponent(s) the following URL:
                        <input #lobby-url style="width: 500px;" readonly='true'>
                        <p> The game will begin once enough players have visited the URL.
                |]
                toWidget
                    [julius|

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
                                else if (serverMessage.command == "joinSuccess")
                                {
                                    var playerId = serverMessage.payload.id;
                                    var gameId = serverMessage.payload.gameId;
                                    handleJoinSuccess(playerId, gameId);
                                }

                            }
                        };

                        var handleGameStarted = function(gameId)
                        {
                            console.info("Game started");
                            window.location = "/game" + "/" + gameId;
                        };

                        var handleJoinSuccess = function(playerId, gameId)
                        {
                            if (playerId && gameId)
                            {
                                 var playerCookie = "id=".concat(playerId).concat(";");
                                 var path = "path=/game/".concat(gameId);

                                 var cookie = playerCookie.concat(path);
                                 console.info("cookie: " + cookie);
                                 document.cookie = cookie;
                            }
                        };

                    |]

    getGameLobby :: Text -> TVar (Map Text (TVar GameLobby)) -> EitherT LobbyResponse STM (TVar GameLobby)
    getGameLobby gameId lobbies = EitherT (note GameDoesNotExist . M.lookup gameId <$> readTVar lobbies)

    getExistingPlayer :: GameLobby -> Text -> Either LobbyResponse ServerPlayer
    getExistingPlayer lobby playerId = note InvalidPlayerID $ find (\player -> identifier player == playerId) players
        where
            players = lobbyPlayers lobby

    {-
        Sets up the broadcast channel for servicing the websocket, returning an ID for the new player
        if they have not joined the lobby before and received a cookie.
    -}
    setupPrequisets :: App -> Text -> Maybe Text -> STM (Either LobbyResponse (Maybe Text, TChan LobbyMessage, Maybe (ServerGame, TChan GameMessage)))
    setupPrequisets app gameId maybePlayerId =
        runEitherT $
            -- TODO: This function probably takes on too many responsibilities
            do
                lobby <- getGameLobby gameId (gameLobbies app)
                oldLobby <- lift $ readTVar lobby
                broadcastChan <- lift . dupTChan . channel $ oldLobby
                case maybePlayerId of
                    Nothing ->
                        do
                            (newPlayer, maybeNewGame) <- lift $ handleJoinNewPlayer app gameId lobby
                            maybeGameSetup <-
                                case maybeNewGame of
                                    Just game -> do
                                        gameChannel <- lift $ dupTChan . broadcastChannel $ game
                                        return $ Just (game, gameChannel)
                                    Nothing -> return Nothing

                            return (Just $ identifier newPlayer, broadcastChan, maybeGameSetup)
                    Just playerId ->
                        hoistEither $ (\_ -> (Nothing, broadcastChan, Nothing)) <$> getExistingPlayer oldLobby playerId

    lobbyWebSocketHandler :: App -> Text -> Maybe Text -> WebSocketsT Handler ()
    lobbyWebSocketHandler app gameId maybePlayerId =
        do
            -- TODO: Too many transaction variables are touched in the initialisation. In particular, the
            -- game / game lobby map at the end could maybe be moved out. Also, I should use the STM containers
            -- map, as it scales better... not that uh, i expect many people will be connecting to this server.
            -- anyway, i digress...
            prequisets <- atomically $ setupPrequisets app gameId maybePlayerId
            case prequisets of
                Left err ->
                    sendTextData $ toJSONResponse err
                Right (maybeNewId, channel, maybeGameSetup) ->
                    do
                        let pool = appConnPool app
                        liftIO $ case maybeGameSetup of
                            Just (game, gameChannel) -> persistNewGame pool gameId game
                            _ -> return ()

                        sendTextData $ toJSONResponse $ (JoinSuccess gameId maybeNewId)
                        {-
                            We race a thread that sends pings to the client so that when the client closes its connection,
                            our loop which reads from the message channel is closed due to the thread being cancelled.
                        -}
                        race_
                            (forever $ (atomically $ toJSONResponse . handleChannelMessage <$> readTChan channel) >>= sendTextData)
                            (whileM_ (isRight <$> sendPingE ("hello~" :: Text)) $ liftIO $ threadDelay 60000000)
