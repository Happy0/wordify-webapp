{-# LANGUAGE ExistentialQuantification #-}
module Repository.GameRepository (UserId, GameSummaryEntity (GameSummaryEntity), GameRepository (getActiveUserGames, getRecentlyActiveGames, getGame), gameSummaryGameId, AnyGameRepository (AnyGameRepository), GameEntity (GameEntity, gameEntityId, gameEntityGame, gameEntityPlayers, gameEntityCreatedAt, gameEntityLastMoveMadeAt, gameEntityFinishedAt, gameEntitySetup)) where

import ClassyPrelude (Bool, UTCTime, Int, Either)
import Data.Maybe
import qualified Data.Text as T
import System.IO
import Controllers.User.Model.ServerUser (ServerUser)
import Model.GameSetup (LocalisedGameSetup)
import Wordify.Rules.Game (Game)

type GameId = T.Text
type UserId = T.Text
type LatestActivity = Maybe UTCTime
type MyMove = Bool
type BoardString = T.Text

data GameSummaryEntity = GameSummaryEntity GameId LatestActivity MyMove BoardString LocalisedGameSetup [ServerUser]

data GameEntity = GameEntity
  { gameEntityId :: GameId
  , gameEntityGame :: Game
  , gameEntityPlayers :: [ServerUser]
  , gameEntityCreatedAt :: UTCTime
  , gameEntityLastMoveMadeAt :: Maybe UTCTime
  , gameEntityFinishedAt :: Maybe UTCTime
  , gameEntitySetup :: LocalisedGameSetup
  }

class GameRepository a where
  getActiveUserGames :: a -> UserId -> IO [GameSummaryEntity]
  getRecentlyActiveGames :: a -> Int -> IO [GameSummaryEntity]
  getGame :: a -> GameId -> IO (Either T.Text GameEntity)

gameSummaryGameId :: GameSummaryEntity -> T.Text
gameSummaryGameId (GameSummaryEntity gameId _ _ _ _ _) = gameId

data AnyGameRepository = forall a. GameRepository a => AnyGameRepository a

instance GameRepository AnyGameRepository where
  getActiveUserGames (AnyGameRepository r) = getActiveUserGames r
  getRecentlyActiveGames (AnyGameRepository r) = getRecentlyActiveGames r
  getGame (AnyGameRepository r) = getGame r