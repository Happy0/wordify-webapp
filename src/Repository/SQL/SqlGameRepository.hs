

module Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend), GameRepository (getActiveUserGames)) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import qualified Model as M
import Repository.GameRepository (GameRepository, GameSummary (GameSummary), UserId, getActiveUserGames)
import Prelude

newtype GameRepositorySQLBackend = GameRepositorySQLBackend (Pool SqlBackend)

instance GameRepository GameRepositorySQLBackend where
  getActiveUserGames (GameRepositorySQLBackend pool) userId = withPool pool (activeUserGames userId)

activeUserGames :: (Monad m, MonadIO m) => UserId -> E.SqlPersistT m [GameSummary]
activeUserGames userId = do
  playerGameEntities <- getActiveUserGamesEntities userId
  playerGameEntitiesWithPlayers <- addAllPlayerGames playerGameEntities
  return $ toGameSummaries playerGameEntitiesWithPlayers

-- TODO - find out how to do 'ARRAY_AGG' or similar to be able to do this within the SQL query rather than doing a 1-to-m fetch
addAllPlayerGames :: (Monad m, MonadIO m) => [(E.Entity M.Player, E.Entity M.Game)] -> E.SqlPersistT m [((E.Entity M.Player, E.Entity M.Game), [E.Entity M.Player])]
addAllPlayerGames = mapM (uncurry getPlayers)
  where
    getPlayers :: (Monad m, MonadIO m) => E.Entity M.Player -> E.Entity M.Game -> E.SqlPersistT m ((E.Entity M.Player, E.Entity M.Game), [E.Entity M.Player])
    getPlayers player game@(E.Entity _ (M.Game gameId _ _ _ _ _ _ _ _)) = do
      allPlayers <- getAllPlayers gameId
      return ((player, game), allPlayers)

getActiveUserGamesEntities :: (Monad m, MonadIO m) => UserId -> E.SqlPersistT m [(E.Entity M.Player, E.Entity M.Game)]
getActiveUserGamesEntities userId =
  E.select $
    E.from $ \(player `E.InnerJoin` game) -> do
      E.on (player ^. M.PlayerGameId E.==. game ^. M.GameGameId)
      E.where_ (player ^. M.PlayerPlayerId E.==. E.val userId)
      E.orderBy         [ E.desc
            (E.coalesce
              [ game ^. M.GameLastMoveMadeAt, E.just (game ^. M.GameCreatedAt)
              ])
        ]
      return (player, game)

getAllPlayers :: (Monad m, MonadIO m) => T.Text -> E.SqlPersistT m [E.Entity M.Player]
getAllPlayers gameId = E.select $
  E.from $ \player -> do
    E.where_ (player ^. M.PlayerGameId E.==. E.val gameId)
    return player

toGameSummaries :: [((E.Entity M.Player, E.Entity M.Game), [E.Entity M.Player])] -> [GameSummary]
toGameSummaries playerGameSummaries =
  -- TODO: check the game is not finished in SQL
  let values = map extractValues playerGameSummaries
   in let activeGames = filter (\(x, y, _) -> isNotFinished y) values
       in map toSummary activeGames
  where
    extractValues :: ((E.Entity M.Player, E.Entity M.Game), [E.Entity M.Player]) -> (M.Player, M.Game, [M.Player])
    extractValues ((E.Entity _ player, E.Entity _ game), allPlayers) = (player, game, map extractValue allPlayers)

    extractValue :: Entity a -> a
    extractValue (E.Entity _ x) = x

    toSummary :: (M.Player, M.Game, [M.Player]) -> GameSummary
    toSummary (M.Player _ _ playerNumber _, M.Game gameId _ _ bagLocale _ _ lastMoveMade currentMoveNumber board, allPlayers) =
      let playable = isPlayerMove currentMoveNumber (length allPlayers) playerNumber
       in GameSummary gameId lastMoveMade playable board (fromMaybe "en" bagLocale)

    isNotFinished :: M.Game -> Bool
    isNotFinished (M.Game gameId _ _ _ _ finishedAt lastMoveMade currentMoveNumber _) = isNothing finishedAt

    isPlayerMove :: Int -> Int -> Int -> Bool
    isPlayerMove currentMoveNumber numberOfPlayers playerNumber =
      case currentMoveNumber `mod` numberOfPlayers of
        x | x > 0 -> x == playerNumber
        x -> playerNumber == numberOfPlayers

withPool = flip runSqlPersistMPool
