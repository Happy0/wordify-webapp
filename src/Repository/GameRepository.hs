{-# LANGUAGE ExistentialQuantification #-}
module Repository.GameRepository (UserId, GameId, MoveNumber, BoardString, PlacedTiles, GameSummaryEntity (GameSummaryEntity), GameRepository (getActiveUserGames, getRecentlyActiveGames, getGame, startNewGame, saveBoardMove, savePassMove, saveExchangeMove, saveGameEnd, updatePlayerLastSeen), gameSummaryGameId, AnyGameRepository (AnyGameRepository), GameEntity (GameEntity, gameEntityId, gameEntityGame, gameEntityPlayers, gameEntityCreatedAt, gameEntityLastMoveMadeAt, gameEntityFinishedAt, gameEntitySetup)) where

import ClassyPrelude (Bool, UTCTime, Int, Either, IO)
import Data.Maybe
import qualified Data.Text as T
import System.IO
import Controllers.User.Model.ServerUser (ServerUser)
import Model.GameSetup (LocalisedGameSetup)
import Wordify.Rules.Game (Game)
import Wordify.Rules.Pos (Pos)
import Wordify.Rules.Tile (Tile)

type GameId = T.Text
type UserId = T.Text
type LatestActivity = Maybe UTCTime
type MyMove = Bool
type BoardString = T.Text
type MoveNumber = Int
type PlacedTiles = [(Pos, Tile)]

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
  startNewGame :: a -> GameEntity -> IO ()
  saveBoardMove :: a -> GameId -> MoveNumber -> PlacedTiles -> BoardString -> IO ()
  savePassMove :: a -> GameId -> MoveNumber -> BoardString -> IO ()
  saveExchangeMove :: a -> GameId -> MoveNumber -> [Tile] -> BoardString -> IO ()
  saveGameEnd :: a -> GameId -> MoveNumber -> Maybe PlacedTiles -> BoardString -> IO ()
  updatePlayerLastSeen :: a -> GameId -> ServerUser -> UTCTime -> IO ()

gameSummaryGameId :: GameSummaryEntity -> T.Text
gameSummaryGameId (GameSummaryEntity gameId _ _ _ _ _) = gameId

data AnyGameRepository = forall a. GameRepository a => AnyGameRepository a

instance GameRepository AnyGameRepository where
  getActiveUserGames (AnyGameRepository r) = getActiveUserGames r
  getRecentlyActiveGames (AnyGameRepository r) = getRecentlyActiveGames r
  getGame (AnyGameRepository r) = getGame r
  startNewGame (AnyGameRepository r) = startNewGame r
  saveBoardMove (AnyGameRepository r) = saveBoardMove r
  savePassMove (AnyGameRepository r) = savePassMove r
  saveExchangeMove (AnyGameRepository r) = saveExchangeMove r
  saveGameEnd (AnyGameRepository r) = saveGameEnd r
  updatePlayerLastSeen (AnyGameRepository r) = updatePlayerLastSeen r