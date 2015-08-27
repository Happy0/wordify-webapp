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
    import Control.Monad.Trans.Maybe

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

    getGameLobby :: Text -> TVar (Map Text (TVar GameLobby)) -> MaybeT STM (TVar GameLobby)
    getGameLobby gameId lobbies = MaybeT (M.lookup gameId <$> readTVar lobbies)

    setupPrequisets :: App -> Text -> STM (Maybe (ServerPlayer, TChan LobbyMessage))
    setupPrequisets app gameId =
        do
            runMaybeT $
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
                Nothing -> 
                    sendTextData $ toJSONResponse GameDoesNotExist
                Just (serverPlayer, channel) ->
                    sendTextData ("arararara~" :: T.Text)