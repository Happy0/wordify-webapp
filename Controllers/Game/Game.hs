module Controllers.Game.Game (createGame) where

    import Control.Monad
    import Data.Text
    import Network.Mail.Mime
    import Prelude
    import System.IO
    import System.Random
    import Controllers.Game.Api
    import Model.Api
    import Model.ServerGame

    createGame :: IO GameCreated
    createGame = 
        do
            newGameId <- (pack . fst . randomString 8 <$> getStdGen)
            -- Create the game lobby here before returning the ID for the game that is being negotiated
            let newGameResponse = GameCreated newGameId
            return newGameResponse