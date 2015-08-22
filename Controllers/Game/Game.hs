module Controllers.Game.Game (performRequest) where

    import Foundation
    import Control.Monad
    import Data.Text
    import Network.Mail.Mime
    import Prelude
    import System.IO
    import System.Random
    import Controllers.Game.Api
    import Model.Api
    import Controllers.Game.Model.ServerGame
    import Wordify.Rules.Player
    import Wordify.Rules.Dictionary
    import Wordify.Rules.LetterBag
    import qualified Data.Map as M
    import qualified Data.Text as T
    import qualified Prelude as P
    import Control.Error.Util
    import Wordify.Rules.Game
    import Data.Bifunctor
    import Control.Monad.Trans.Either
    import Control.Monad.Trans.Class
    import Control.Concurrent.STM
    import Controllers.Game.Model.GameLobby

    performRequest :: App -> ClientRequest -> IO (Either Text ServerResponse)
    performRequest app (CreateGameRequest players) = createGame app players "en"

    createGame :: App -> Int -> Locale -> IO (Either Text ServerResponse)
    createGame app numPlayers locale =
        runEitherT $
            do
                    newGameId <- lift $ T.pack . fst . randomString 8 <$> getStdGen
                    newGame <- setupGame app locale numPlayers
                    let lobbies = gameLobbies app
                    lift $ addGameLobby app newGameId newGame numPlayers

                    return $ GameCreated newGameId

    addGameLobby :: App -> Text -> Game -> Int -> IO ()
    addGameLobby app gameId game numPlayers = atomically $ do
        let lobbies = gameLobbies app
        gameLobby <- newTVar (GameLobby game [] numPlayers)
        modifyTVar lobbies $ M.insert gameId gameLobby

    setupGame :: App -> Locale -> Int -> (EitherT Text IO Game)
    setupGame app locale numPlayers =
            do
                let gameSetups = localisedGameSetups app 
                players <- hoistEither $ createPlayers numPlayers
                (dictionary, letterbag) <- hoistEither $ getLetterbagAndDictionary locale gameSetups

                -- Randomise the order of the letters in the letter bag
                shuffledLetterbag <- lift $ shuffleWithNewGenerator letterbag

                -- Convert a ScrabbleError into a string to return to the client if we failed to create the game
                -- This should never happen when setting up a new game
                hoistEither $ first (T.pack . show) $ makeGame players shuffledLetterbag dictionary

    getLetterbagAndDictionary :: Locale -> LocalisedGameSetups -> Either Text (Dictionary, LetterBag)
    getLetterbagAndDictionary locale setups = 
        do 
            setup <- note "Invalid locale" $ M.lookup locale setups
            return (localisedDictionary setup, localisedLetterBag setup)

    createPlayers :: Int -> Either Text (Player, Player, Maybe (Player, Maybe Player))
    createPlayers numPlayers
        | numPlayers == 2 = Right (makePlayer "player1", makePlayer "player2", Nothing)
        | numPlayers == 3 = Right (makePlayer "player1", makePlayer "player2", Just ((makePlayer "player3"), Nothing))
        | numPlayers == 4 = Right (makePlayer "player1", makePlayer "player2", Just ((makePlayer "player3"), Just (makePlayer "player4")))
        | otherwise = Left "Invalid number of players"