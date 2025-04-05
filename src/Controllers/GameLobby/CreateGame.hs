module Controllers.GameLobby.CreateGame (createGame) where

import ClassyPrelude.Yesod (getYesod, newTVarIO)
import Control.Concurrent.STM
import Control.Error.Util
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Controllers.Game.Model.ServerGame
import Controllers.Game.Persist
import Controllers.GameLobby.Model.GameLobby
import Data.Bifunctor
import qualified Data.Map as M
import Data.Text
import qualified Data.Text as T
import Data.Time.Clock
import Foundation
import Model.Api
import Network.Mail.Mime
import System.IO
import System.Random
import Wordify.Rules.Dictionary
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Player
import Prelude
import qualified Prelude as P

createGame :: App -> Int -> Locale -> IO (Either Text Text)
createGame app numPlayers locale =
  runExceptT $
    do
      newGameId <- lift $ T.pack . fst . randomString 8 <$> getStdGen
      newGame <- setupGame app locale numPlayers
      initialLobby <- lift $ createGameLobby app newGameId newGame numPlayers
      _ <- lift $ persistNewLobby (appConnPool app) newGameId locale initialLobby
      return newGameId

createGameLobby :: App -> Text -> Game -> Int -> IO GameLobby
createGameLobby app gameId game numPlayers =
  do
    generator <- getStdGen
    timeCreated <- getCurrentTime
    atomically $ do
      let lobbies = gameLobbies app
      broadcastChan <- newBroadcastTChan
      newGenerator <- newTVar generator
      serverPlayers <- newTVar []
      let initialLobby = GameLobby game serverPlayers numPlayers broadcastChan newGenerator timeCreated
      return initialLobby

setupGame :: App -> Locale -> Int -> (ExceptT Text IO Game)
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
  | otherwise = Left $ T.concat (["Invalid number of players: ", pack (show numPlayers)])
