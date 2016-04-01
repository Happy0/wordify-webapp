module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Wordify.Rules.Tile
import qualified Data.List.NonEmpty as NE
import Wordify.Rules.Board
import Wordify.Rules.Move
import Wordify.Rules.Game
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text as T
import Network.Mail.Mime
import System.Random
import Model.Api
import Data.Aeson
import Web.Cookie

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidget
            [hamlet|
              <div>
            |]
