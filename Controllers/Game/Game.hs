module Controllers.Game.Game (createGame) where

    import Control.Monad
    import Data.Text
    import Network.Mail.Mime
    import Prelude
    import System.IO
    import System.Random

    createGame :: IO Text
    createGame = 
        do
            newGameId <- (pack . fst . randomString 8 <$> getStdGen)
            -- Create the game lobby here before returning the ID for the game that is being negotiated
            return newGameId