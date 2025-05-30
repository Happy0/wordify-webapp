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
import Model.GameSetup( localisedDictionary, localisedLetterBag, LocalisedGameSetup)

createGame :: App -> Int -> Locale -> IO (Either Text Text)
createGame app numPlayers locale =
  runExceptT $
    do
      let gameSetups = localisedGameSetups app
      localeSetup <- hoistEither $ getLocaleSetup locale gameSetups
      newGameId <- lift $ T.pack . fst . randomString 8 <$> getStdGen
      newGame <- setupGame localeSetup numPlayers
      initialLobby <- lift $ createGameLobby app newGameId newGame localeSetup numPlayers locale
      _ <- lift $ persistNewLobby (appConnPool app) newGameId locale initialLobby
      return newGameId

createGameLobby :: App -> Text -> Game -> LocalisedGameSetup -> Int -> Text -> IO GameLobby
createGameLobby app gameId game localisedGameSetup numPlayers gameLanguage =
  do
    generator <- getStdGen
    timeCreated <- getCurrentTime
    atomically $ do
      let lobbies = gameLobbies app
      broadcastChan <- newBroadcastTChan
      newGenerator <- newTVar generator
      serverPlayers <- newTVar []
      let initialLobby = GameLobby game serverPlayers numPlayers broadcastChan newGenerator timeCreated gameLanguage localisedGameSetup
      return initialLobby

setupGame :: LocalisedGameSetup -> Int -> ExceptT Text IO Game
setupGame localisedGameSetup numPlayers =
  do
    players <- hoistEither $ createPlayers numPlayers

    -- Randomise the order of the letters in the letter bag
    shuffledLetterbag <- lift $ shuffleWithNewGenerator (localisedLetterBag localisedGameSetup)

    -- Convert a ScrabbleError into a string to return to the client if we failed to create the game
    -- This should never happen when setting up a new game
    hoistEither $ first (T.pack . show) $ makeGame players shuffledLetterbag (localisedDictionary localisedGameSetup)

getLocaleSetup :: Locale -> LocalisedGameSetups -> Either Text LocalisedGameSetup
getLocaleSetup locale setups =
  note (T.concat ["Invalid locale: ", locale]) $ M.lookup locale setups

createPlayers :: Int -> Either Text (Player, Player, Maybe (Player, Maybe Player))
createPlayers numPlayers
  | numPlayers == 2 = Right (makePlayer "player1", makePlayer "player2", Nothing)
  | numPlayers == 3 = Right (makePlayer "player1", makePlayer "player2", Just ((makePlayer "player3"), Nothing))
  | numPlayers == 4 = Right (makePlayer "player1", makePlayer "player2", Just ((makePlayer "player3"), Just (makePlayer "player4")))
  | otherwise = Left $ T.concat (["Invalid number of players: ", pack (show numPlayers)])
