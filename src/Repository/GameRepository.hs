module Repository.GameRepository (UserId, GameSummary (GameSummary), OtherPlayerNames, GameRepository (getActiveUserGames)) where

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
type PlayerName = T.Text
type OtherPlayerNames = [T.Text]

data GameSummary = GameSummary GameId LatestActivity MyMove BoardString LocaleString OtherPlayerNames

class GameRepository a where
  getActiveUserGames :: a -> UserId -> IO [GameSummary]