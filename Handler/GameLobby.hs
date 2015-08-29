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

            webSockets $ lobbyWebSocketHandler app gameId
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

    setupPrequisets :: App -> Text -> STM (Either LobbyResponse (ServerPlayer, TChan LobbyMessage))
    setupPrequisets app gameId =
        do
            runEitherT $
                do
                    lobby <- getGameLobby gameId (gameLobbies app)
                    newPlayer <- lift $ handleJoinNewPlayer lobby
                    updatedLobby <- lift $ readTVar lobby
                    broadcastChan <- lift $ dupTChan (channel updatedLobby)
                    
                    lift $ writeTChan broadcastChan $ PlayerJoined newPlayer
                    lift $ handleLobbyFull app updatedLobby gameId

                    return (newPlayer, broadcastChan)

    lobbyWebSocketHandler :: App -> Text -> WebSocketsT Handler ()
    lobbyWebSocketHandler app gameId =
        do
            prequisets <- atomically $ setupPrequisets app gameId

            case prequisets of
                Left err -> 
                    sendTextData $ toJSONResponse err
                Right (serverPlayer, channel) ->
                    sendTextData ("arararara~" :: T.Text)