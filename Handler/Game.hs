module Handler.Game where

import Import
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import qualified Data.Monoid as M
import Data.Text (Text)
import Controllers.Game.Model.ServerGame
import Widgets.Game.Game

getGameR :: Text -> Handler Html
getGameR gameId = do
    request <- getRequest
    app <- getYesod
    let cookies = reqCookies request
    let maybePlayerId = lookup "id" cookies
    let gamesInProgress = games app
    maybeGame <- atomically $ lookup gameId <$> readTVar gamesInProgress

    case maybeGame of
        Nothing -> notFound
        Just game ->
            do
                webSockets $ gameApp game maybePlayerId
                defaultLayout $ do
                    [whamlet|
                        ^{emptyGame}
                    |]
                    toWidget
                        [julius|
                            var url = document.URL,

                            url = url.replace("http:", "ws:").replace("https:", "wss:");
                            var conn = new WebSocket(url);

                            conn.onmessage = function(e) {
                                console.dir(e);
                            };

                        |]

gameApp :: TVar ServerGame -> Maybe Text -> WebSocketsT Handler ()
gameApp game maybePlayerId =
    do
        sendPing ("testarooni" :: Text)


{-testApp :: WebSocketsT Handler ()
testApp = do
    sendTextData ("blah blah message 1" :: Text)
    sendTextData $ (("blah blah message 2") :: Text)
    redirect HomeR
-}