

module Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend), GameRepository (getActiveUserGames)) where

import Control.Monad.IO.Class (MonadIO)
import Data.List (groupBy)
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
  rows <- E.select $
    E.from $ \(me `E.InnerJoin` game `E.LeftOuterJoin` other `E.LeftOuterJoin` usr) -> do
      E.on (usr ^. M.UserIdent E.==. other ^. M.PlayerPlayerId)
      E.on (other ^. M.PlayerGameId E.==. game ^. M.GameGameId
            E.&&. other ^. M.PlayerPlayerId E.!=. E.val userId)
      E.on (me ^. M.PlayerGameId E.==. game ^. M.GameGameId)
      E.where_ (me ^. M.PlayerPlayerId E.==. E.val userId)
      E.orderBy
        [ E.desc
            (E.coalesce
              [ game ^. M.GameLastMoveMadeAt, E.just (game ^. M.GameCreatedAt)
              ])
        ]
      return (me, game, E.just (usr ^. M.UserNickname))
  return $ toGameSummaries rows

toGameSummaries :: [(E.Entity M.Player, E.Entity M.Game, E.Value (Maybe (Maybe T.Text)))] -> [GameSummary]
toGameSummaries rows =
  let grouped = groupBy (\(_, E.Entity _ g1, _) (_, E.Entity _ g2, _) -> M.gameGameId g1 == M.gameGameId g2) rows
   in mapMaybe groupToSummary grouped
  where
    groupToSummary :: [(E.Entity M.Player, E.Entity M.Game, E.Value (Maybe (Maybe T.Text)))] -> Maybe GameSummary
    groupToSummary [] = Nothing
    groupToSummary group@((E.Entity _ player, E.Entity _ game, _):_) =
      if isNothing (M.gameFinishedAt game)
        then
          let otherNames = concatMap extractName group
              totalPlayers = length otherNames + 1
              playable = isPlayerMove (M.gameCurrentMoveNumber game) totalPlayers (M.playerPlayerNumber player)
           in Just $ GameSummary
                (M.gameGameId game)
                (M.gameLastMoveMadeAt game)
                playable
                (M.gameBoard game)
                (fromMaybe "en" (M.gameBagLocale game))
                otherNames
        else Nothing

    extractName :: (E.Entity M.Player, E.Entity M.Game, E.Value (Maybe (Maybe T.Text))) -> [T.Text]
    extractName (_, _, E.Value Nothing) = []
    extractName (_, _, E.Value (Just nickname)) = [fromMaybe "<Unknown>" nickname]

    isPlayerMove :: Int -> Int -> Int -> Bool
    isPlayerMove currentMoveNumber numberOfPlayers playerNumber =
      case currentMoveNumber `mod` numberOfPlayers of
        x | x > 0 -> x == playerNumber
        _ -> playerNumber == numberOfPlayers

withPool = flip runSqlPersistMPool
