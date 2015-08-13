module Controllers.Game.Game (createGame) where

    import Control.Monad
    import Data.Text
    import Network.Mail.Mime
    import Prelude
    import System.IO
    import System.Random

    createGame :: IO Text
    createGame = (pack . fst . randomString 8 <$> getStdGen)