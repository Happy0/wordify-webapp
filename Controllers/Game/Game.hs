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
    import Wordify.Rules.Player

    performRequest :: ClientRequest -> IO ServerResponse
    performRequest (CreateGameRequest players) = createGame players

    createGame :: Int -> IO ServerResponse
    createGame players = 
        do
            newGameId <- pack . fst . randomString 8 <$> getStdGen
            -- Create the game lobby here before returning the ID for the game that is being negotiated
            let newGameResponse = GameCreated newGameId
            return newGameResponse

    createPlayers :: Int -> Maybe (Player, Player, Maybe (Player, Maybe Player))
    createPlayers numPlayers
        | numPlayers == 2 = Just (makePlayer "player1", makePlayer "player2", Nothing)
        | numPlayers == 3 = Just (makePlayer "player1", makePlayer "player2", Just ((makePlayer "player3"), Nothing))
        | numPlayers == 4 = Just (makePlayer "player1", makePlayer "player2", Just ((makePlayer "player3"), Just (makePlayer "player4")))
        | otherwise = Nothing