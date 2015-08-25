module Handler.Game where

import Import
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import qualified Data.Monoid as M
import Data.Text (Text)

getGameR :: Text -> Handler Html
getGameR string = do
    request <- getRequest
    let cookies = reqCookies request
    -- webSockets testApp
    defaultLayout $ do
        [whamlet|
            $forall cookie <- cookies
                <p> #{show cookie}
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


{-testApp :: WebSocketsT Handler ()
testApp = do
    sendTextData ("blah blah message 1" :: Text)
    sendTextData $ (("blah blah message 2") :: Text)
    redirect HomeR
-}