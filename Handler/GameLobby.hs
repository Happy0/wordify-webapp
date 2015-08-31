module Handler.GameLobby where

    import Import
    import Foundation
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
    import Control.Monad
    import Control.Monad.Trans.Either
    import Control.Error.Util

    getGameLobbyR :: Text -> Handler Html
    getGameLobbyR gameId =
        do
            app <- getYesod
            maybePlayerId <- lookupCookie "id"

            webSockets $ lobbyWebSocketHandler app gameId maybePlayerId
            defaultLayout $ do
                [whamlet|
                    <div .lobby>

                |]
                toWidget
                    [julius|

                        var url = document.URL;
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
                                 var path = "path=/game/".concat(gameId).concat("/");

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
    setupPrequisets :: App -> Text -> Maybe Text -> STM (Either LobbyResponse (Maybe Text, TChan LobbyMessage))
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
                            newPlayer <- lift $ handleJoinNewPlayer app gameId lobby
                            return (Just $ identifier newPlayer, broadcastChan)
                    Just playerId ->
                        hoistEither $ (\_ -> (Nothing, broadcastChan)) <$> getExistingPlayer oldLobby playerId

    lobbyWebSocketHandler :: App -> Text -> Maybe Text -> WebSocketsT Handler ()
    lobbyWebSocketHandler app gameId maybePlayerId =
        do
            prequisets <- atomically $ setupPrequisets app gameId maybePlayerId
            case prequisets of
                Left err -> 
                    sendTextData $ toJSONResponse err
                Right (maybeNewId, channel) ->
                    do
                        sendTextData $ toJSONResponse $ (JoinSuccess gameId maybeNewId)
                        forever $ (atomically $ toJSONResponse . handleChannelMessage <$> readTChan channel) >>= sendTextData