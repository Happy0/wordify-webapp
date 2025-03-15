module Repository.GameRepository (UserId, GameSummary (GameSummary), GameRepository (getActiveUserGames)) where

import ClassyPrelude (Bool, UTCTime)
import Data.Maybe
import qualified Data.Text as T
import System.IO

type GameId = T.Text

type UserId = T.Text

type LatestActivity = Maybe UTCTime

type MyMove = Bool

data GameSummary = GameSummary GameId LatestActivity MyMove

class GameRepository a where
  getActiveUserGames :: a -> UserId -> IO [GameSummary]