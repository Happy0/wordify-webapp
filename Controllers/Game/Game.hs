module Controllers.Game.Game (performRequest) where

    import Control.Monad
    import Data.Text
    import Network.Mail.Mime
    import Prelude
    import System.IO
    import System.Random
    import Controllers.Game.Api
    import Model.Api
    import Model.ServerGame    

    performRequest :: ClientRequest -> IO ServerResponse
    performRequest (CreateGameRequest players) = createGame players

    createGame :: Int -> IO ServerResponse
    createGame players = 
        do
            newGameId <- (pack . fst . randomString 8 <$> getStdGen)
            -- Create the game lobby here before returning the ID for the game that is being negotiated
            let newGameResponse = GameCreated newGameId
            return newGameResponse