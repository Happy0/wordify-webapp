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

            webSockets $ lobbyWebSocketHandler app gameId Nothing
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
                            alert(e.data);
                        };

                    |]

    getGameLobby :: Text -> TVar (Map Text (TVar GameLobby)) -> EitherT LobbyResponse STM (TVar GameLobby)
    getGameLobby gameId lobbies = EitherT (note GameDoesNotExist . M.lookup gameId <$> readTVar lobbies)

    getExistingPlayer :: GameLobby -> Text -> Either LobbyResponse ServerPlayer
    getExistingPlayer lobby playerId = note InvalidPlayerID $ find (\player -> identifier player == playerId) players
        where
            players = lobbyPlayers lobby

    setupPrequisets :: App -> Text -> Maybe Text -> STM (Either LobbyResponse (ServerPlayer, TChan LobbyMessage))
    setupPrequisets app gameId maybePlayerId =
        runEitherT $
            do
                lobby <- getGameLobby gameId (gameLobbies app)
                oldLobby <- lift $ readTVar lobby
                broadcastChan <- lift . dupTChan . channel $ oldLobby
                case maybePlayerId of
                    Nothing ->
                        do
                            newPlayer <- lift $ handleJoinNewPlayer lobby
                            updatedLobby <- lift $ readTVar lobby
                            lift $ writeTChan broadcastChan $ PlayerJoined newPlayer
                            lift $ handleLobbyFull app updatedLobby gameId
                            return (newPlayer, broadcastChan)
                    Just playerId ->
                        hoistEither $ (\player -> (player, broadcastChan)) <$> getExistingPlayer oldLobby playerId

    lobbyWebSocketHandler :: App -> Text -> Maybe Text -> WebSocketsT Handler ()
    lobbyWebSocketHandler app gameId maybePlayerId =
        do
            prequisets <- atomically $ setupPrequisets app gameId maybePlayerId
            case prequisets of
                Left err -> 
                    sendTextData $ toJSONResponse err
                Right (serverPlayer, channel) ->
                    sendTextData $ toJSONResponse JoinSuccess