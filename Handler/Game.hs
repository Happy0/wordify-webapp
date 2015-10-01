module Handler.Game where

import Import
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import qualified Data.Monoid as M
import Data.Text (Text)
import Controllers.Game.Model.ServerGame
import Widgets.Game.Game
import Controllers.Game.Api
import qualified Data.List as L
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Api
import Model.Api
import Control.Concurrent

getGameR :: Text -> Handler Html
getGameR gameId = do
    request <- getRequest
    app <- getYesod
    let cookies = reqCookies request
    let maybePlayerId = L.lookup "id" cookies
    let gamesInProgress = games app
    maybeGame <- atomically $ lookup gameId <$> readTVar gamesInProgress

    case maybeGame of
        Nothing -> notFound
        Just game ->
            do
                (currentGame, messageChannel) <- atomically $ setupPrerequisets game
                let maybePlayerNumber = maybePlayerId >>= getPlayerNumber currentGame
                webSockets $ gameApp game messageChannel maybePlayerId maybePlayerNumber
                defaultLayout $ do
                    [whamlet|
                        ^{gameInPlay currentGame maybePlayerNumber}
                    |]
                    controller

controller :: Widget
controller =
        toWidget
            [julius|
                var url = document.URL,

                url = url.replace("http:", "ws:").replace("https:", "wss:");
                var conn = new WebSocket(url);

                conn.onMessage = function (e) {
                    parseServerMessage(e.data);
                }

                var parseServerMessage = function(serverMessage)
                {
                    console.dir(serverMessage);

                    if (serverMessage && serverMessage.command)
                    {

                        if (serverMessage.command === "said")
                        {
                            var sender = serverMessage.payload.name;
                            var message = serverMessage.payload.message;

                            chat.controller.messageRecieved(sender, message);
                        }

                    }
                };

            |]

getPlayerNumber :: ServerGame -> Text -> Maybe Int
getPlayerNumber serverGame playerId = fst <$> (L.find (\(ind, player) -> playerId == identifier player) $ zip [1 .. 4] players)
    where
        players = playing serverGame

setupPrerequisets :: TVar ServerGame -> STM (ServerGame, TChan GameMessage)
setupPrerequisets serverGame =
    do
        game <- readTVar serverGame
        channel <- dupTChan $ broadcastChannel game
        return (game, channel)


gameApp :: TVar ServerGame -> TChan GameMessage -> Maybe Text -> Maybe Int -> WebSocketsT Handler ()
gameApp game channel maybePlayerId playerNumber =
    do
        sendTextData $ toJSONResponse $ PlayerSaid "sender" "message123"
        liftIO $ threadDelay 20000000


{-testApp :: WebSocketsT Handler ()
testApp = do
    sendTextData ("blah blah message 1" :: Text)
    sendTextData $ (("blah blah message 2") :: Text)
    redirect HomeR
-}