module Repository.GameRepository (UserId, GameSummary (GameSummary), GameRepository (getActiveUserGames)) where

import ClassyPrelude (Bool, UTCTime)
import Data.Maybe
import qualified Data.Text as T
import System.IO

type GameId = T.Text

type UserId = T.Text

type LatestActivity = Maybe UTCTime

type MyMove = Bool

type BoardString = T.Text

type LocaleString = T.Text

data GameSummary = GameSummary GameId LatestActivity MyMove BoardString LocaleString

class GameRepository a where
  getActiveUserGames :: a -> UserId -> IO [GameSummary]