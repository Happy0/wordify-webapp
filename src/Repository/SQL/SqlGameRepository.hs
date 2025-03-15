{-# LANGUAGE TypeApplications #-}

module Repository.SQL.SqlGameRepository (SqlPool, GameRepository (getActiveUserGames)) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe
import Data.Pool
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import Import (GameId, YesodPersist (runDB))
import qualified Model as M
import Repository.GameRepository (GameRepository, GameSummary (GameSummary), UserId, getActiveUserGames)
import System.IO
import Prelude

data SqlPool = SqlPool (Pool SqlBackend)

instance GameRepository SqlPool where
  getActiveUserGames (SqlPool pool) userId = withPool pool (activeUserGames userId)

activeUserGames :: (Monad m, MonadIO m) => UserId -> E.SqlPersistT m [GameSummary]
activeUserGames userId = do
  playerGameEntities <- getActiveUserGamesEntities userId
  return $ toGameSummaries playerGameEntities

getActiveUserGamesEntities :: (Monad m, MonadIO m) => UserId -> E.SqlPersistT m [(E.Entity M.Player, E.Entity M.Game)]
getActiveUserGamesEntities userId =
  E.select $
    E.from $ \(player `E.InnerJoin` game) -> do
      E.on (player ^. M.PlayerGameId E.==. game ^. M.GameGameId)
      E.where_ (player ^. M.PlayerPlayerId E.==. E.val userId)
      return (player, game)

toGameSummaries :: [(E.Entity M.Player, E.Entity M.Game)] -> [GameSummary]
toGameSummaries playerGameSummaries =
  let values = map extractValues playerGameSummaries
   in -- TODO: do this in SQL query
      let activeGames = filter (\(x, y) -> isNotFinished y) values
       in map (uncurry toSummary) activeGames
  where
    extractValues :: (E.Entity M.Player, E.Entity M.Game) -> (M.Player, M.Game)
    extractValues ((E.Entity _ player, E.Entity _ game)) = (player, game)

    toSummary :: M.Player -> M.Game -> GameSummary
    toSummary (M.Player _ _ playerNumber _) (M.Game gameId _ _ _ _ _ lastMoveMade currentMoveNumber) =
      GameSummary gameId lastMoveMade False -- TODO: use this field properly
    isNotFinished :: M.Game -> Bool
    isNotFinished (M.Game gameId _ _ _ _ finishedAt lastMoveMade currentMoveNumber) = isNothing finishedAt

withPool pool = flip runSqlPersistMPool pool
