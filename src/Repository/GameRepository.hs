module Repository.GameRepository (UserId, GameSummary (GameSummary), OtherPlayerNames, GameRepository (getActiveUserGames), gameSummaryGameId) where

import ClassyPrelude (Bool, UTCTime)
import Data.Maybe
import qualified Data.Text as T
import System.IO
import Controllers.Game.Model.ServerGame (ServerGame(gameId))
import Model.GameSetup (LocalisedGameSetup)

type GameId = T.Text
type UserId = T.Text
type LatestActivity = Maybe UTCTime
type MyMove = Bool
type BoardString = T.Text
type OtherPlayerNames = [T.Text]

data GameSummary = GameSummary GameId LatestActivity MyMove BoardString LocalisedGameSetup OtherPlayerNames

class GameRepository a where
  getActiveUserGames :: a -> UserId -> IO [GameSummary]

gameSummaryGameId :: GameSummary -> T.Text
gameSummaryGameId (GameSummary gameId _ _ _ _ _) = gameId