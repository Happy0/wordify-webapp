{-# LANGUAGE ExistentialQuantification #-}
module Repository.GameRepository (UserId, GameSummaryEntity (GameSummaryEntity), GameRepository (getActiveUserGames, getRecentlyActiveGames), gameSummaryGameId, AnyGameRepository (AnyGameRepository)) where

import ClassyPrelude (Bool, UTCTime, Int)
import Data.Maybe
import qualified Data.Text as T
import System.IO
import Controllers.Game.Model.ServerGame (ServerGame(gameId))
import Controllers.User.Model.ServerUser (ServerUser)
import Model.GameSetup (LocalisedGameSetup)

type GameId = T.Text
type UserId = T.Text
type LatestActivity = Maybe UTCTime
type MyMove = Bool
type BoardString = T.Text

data GameSummaryEntity = GameSummaryEntity GameId LatestActivity MyMove BoardString LocalisedGameSetup [ServerUser]

 

class GameRepository a where
  getActiveUserGames :: a -> UserId -> IO [GameSummaryEntity]
  getRecentlyActiveGames :: a -> Int -> IO [GameSummaryEntity]

gameSummaryGameId :: GameSummaryEntity -> T.Text
gameSummaryGameId (GameSummaryEntity gameId _ _ _ _ _) = gameId

data AnyGameRepository = forall a. GameRepository a => AnyGameRepository a

instance GameRepository AnyGameRepository where
  getActiveUserGames (AnyGameRepository r) = getActiveUserGames r
  getRecentlyActiveGames (AnyGameRepository r) = getRecentlyActiveGames r