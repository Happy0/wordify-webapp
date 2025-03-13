{-# LANGUAGE TypeApplications #-}

module Repository.SQL.SqlGameRepository (SqlPool) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe
import Data.Pool
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import Import (GameId)
import qualified Model as M
import Repository.GameRepository (GameRepository, GameSummary (GameSummary), UserId, getActiveUserGames)
import System.IO
import Prelude

data SqlPool = SqlPool (Pool SqlBackend)

instance GameRepository SqlPool where
  getActiveUserGames pool userId = undefined

getActiveUserGames :: (Monad m, MonadIO m) => ConnectionPool -> UserId -> E.SqlPersistT m [GameSummary]
getActiveUserGames pool userId = do
  result <- E.select $
    E.from $ \(player `E.InnerJoin` game) -> do
      E.on (player ^. M.PlayerGameId E.==. game ^. M.GameGameId)
      E.where_ (player ^. M.PlayerPlayerId E.==. E.val userId)
      return (player, game)

  return $ toGameSummaries result

toGameSummaries :: [(E.Entity M.Player, E.Entity M.Game)] -> [GameSummary]
toGameSummaries summaries =
  let values = map extractValues summaries
   in -- TODO: do this in SQL query
      let activeGames = filter (\(x, y) -> isNotFinished y) values
       in map (uncurry toSummary) activeGames
  where
    extractValues :: (E.Entity M.Player, E.Entity M.Game) -> (M.Player, M.Game)
    extractValues (x, y) = undefined

    toSummary :: M.Player -> M.Game -> GameSummary
    toSummary (M.Player _ _ playerNumber _) (M.Game gameId _ _ _ _ _ lastMoveMade currentMoveNumber) = undefined

    isNotFinished :: M.Game -> Bool
    isNotFinished (M.Game gameId _ _ _ _ finishedAt lastMoveMade currentMoveNumber) = isNothing finishedAt

withPool pool = flip runSqlPersistMPool pool
