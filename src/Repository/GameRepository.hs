module Repository.GameRepository (UserId, GameSummary (GameSummary), GameRepository (getActiveUserGames)) where

import ClassyPrelude (Bool, UTCTime)
import qualified Data.Text as T
import System.IO
import Wordify.Rules.Board (Board)

type GameId = T.Text

type UserId = T.Text

type LatestActivity = UTCTime

type MyMove = Bool

data GameSummary = GameSummary GameId LatestActivity MyMove

class GameRepository a where
  getActiveUserGames :: a -> UserId -> IO [GameSummary]