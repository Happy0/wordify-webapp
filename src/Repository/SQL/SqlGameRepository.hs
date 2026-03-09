

module Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend), GameRepository (getActiveUserGames, getRecentlyActiveGames)) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO)
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import qualified Model as M
import Model.GameSetup (LocalisedGameSetup)
import Repository.GameRepository (GameRepository, GameSummaryEntity (GameSummaryEntity), UserId, getActiveUserGames, getRecentlyActiveGames)
import Controllers.User.Model.ServerUser (ServerUser (ServerUser))
import Prelude

data GameRepositorySQLBackend = GameRepositorySQLBackend (Pool SqlBackend) (Map.Map T.Text LocalisedGameSetup)

instance GameRepository GameRepositorySQLBackend where
  getActiveUserGames (GameRepositorySQLBackend pool gameSetups) userId = withPool pool (activeUserGames gameSetups userId)
  getRecentlyActiveGames (GameRepositorySQLBackend pool gameSetups) n = withPool pool (recentlyActiveGames gameSetups n)

activeUserGames :: (Monad m, MonadIO m) => Map.Map T.Text LocalisedGameSetup -> UserId -> E.SqlPersistT m [GameSummaryEntity]
activeUserGames gameSetups userId = do
  rows <- E.select $
    E.from $ \(me `E.InnerJoin` game `E.LeftOuterJoin` other `E.LeftOuterJoin` usr) -> do
      E.on (usr ^. M.UserIdent E.==. other ^. M.PlayerPlayerId)
      E.on (other ^. M.PlayerGameId E.==. game ^. M.GameGameId)
      E.on (me ^. M.PlayerGameId E.==. game ^. M.GameGameId)
      E.where_ (me ^. M.PlayerPlayerId E.==. E.val userId)
      E.orderBy
        [ E.desc
            (E.coalesce
              [ game ^. M.GameLastMoveMadeAt, E.just (game ^. M.GameCreatedAt)
              ])
        ]
      return (me, game, E.just (other ^. M.PlayerPlayerId), usr ^. M.UserUsername)
  return $ toGameSummaries gameSetups rows

toGameSummaries :: Map.Map T.Text LocalisedGameSetup -> [(E.Entity M.Player, E.Entity M.Game, E.Value (Maybe T.Text), E.Value (Maybe T.Text))] -> [GameSummaryEntity]
toGameSummaries gameSetups rows =
  let grouped = groupBy (\(_, E.Entity _ g1, _, _) (_, E.Entity _ g2, _, _) -> M.gameGameId g1 == M.gameGameId g2) rows
   in mapMaybe groupToSummary grouped
  where
    groupToSummary :: [(E.Entity M.Player, E.Entity M.Game, E.Value (Maybe T.Text), E.Value (Maybe T.Text))] -> Maybe GameSummaryEntity
    groupToSummary [] = Nothing
    groupToSummary group@((E.Entity _ player, E.Entity _ game, _, _):_) =
      if isNothing (M.gameFinishedAt game)
        then do
          let localeCode = fromMaybe "en" (M.gameBagLocale game)
          localisedSetup <- Map.lookup localeCode gameSetups
          let players = concatMap extractPlayer group
              totalPlayers = length players
              playable = isPlayerMove (M.gameCurrentMoveNumber game) totalPlayers (M.playerPlayerNumber player)
          Just $ GameSummaryEntity
                (M.gameGameId game)
                (M.gameLastMoveMadeAt game <|> Just (M.gameCreatedAt game))
                playable
                (M.gameBoard game)
                localisedSetup
                players
        else Nothing

    extractPlayer :: (E.Entity M.Player, E.Entity M.Game, E.Value (Maybe T.Text), E.Value (Maybe T.Text)) -> [ServerUser]
    extractPlayer (_, _, E.Value Nothing, _) = []  -- no player in this row (LEFT JOIN produced NULL)
    extractPlayer (_, _, E.Value (Just pid), E.Value uname) = [ServerUser pid uname]

    isPlayerMove :: Int -> Int -> Int -> Bool
    isPlayerMove currentMoveNumber numberOfPlayers playerNumber =
      case currentMoveNumber `mod` numberOfPlayers of
        x | x > 0 -> x == playerNumber
        _ -> playerNumber == numberOfPlayers

recentlyActiveGames :: (Monad m, MonadIO m) => Map.Map T.Text LocalisedGameSetup -> Int -> E.SqlPersistT m [GameSummaryEntity]
recentlyActiveGames gameSetups n = do
  topGameIds <- E.select $ E.from $ \game -> do
    E.orderBy [E.desc (E.coalesce [game ^. M.GameLastMoveMadeAt, E.just (game ^. M.GameCreatedAt)])]
    E.limit (fromIntegral n)
    return (game ^. M.GameGameId)
  let ids = map E.unValue topGameIds
  if null ids
    then return []
    else do
      rows <- E.select $ E.from $ \(game `E.InnerJoin` player `E.LeftOuterJoin` usr) -> do
        E.on (usr ^. M.UserIdent E.==. player ^. M.PlayerPlayerId)
        E.on (player ^. M.PlayerGameId E.==. game ^. M.GameGameId)
        E.where_ (game ^. M.GameGameId `E.in_` E.valList ids)
        E.orderBy [E.desc (E.coalesce [game ^. M.GameLastMoveMadeAt, E.just (game ^. M.GameCreatedAt)])]
        return (game, player ^. M.PlayerPlayerId, usr ^. M.UserUsername)
      return $ toGlobalGameSummaries gameSetups rows

toGlobalGameSummaries :: Map.Map T.Text LocalisedGameSetup -> [(E.Entity M.Game, E.Value T.Text, E.Value (Maybe T.Text))] -> [GameSummaryEntity]
toGlobalGameSummaries gameSetups rows =
  let grouped = groupBy (\(E.Entity _ g1, _, _) (E.Entity _ g2, _, _) -> M.gameGameId g1 == M.gameGameId g2) rows
   in mapMaybe groupToSummary grouped
  where
    groupToSummary [] = Nothing
    groupToSummary group@((E.Entity _ game, _, _):_) = do
      let localeCode = fromMaybe "en" (M.gameBagLocale game)
      localisedSetup <- Map.lookup localeCode gameSetups
      let players = map (\(_, E.Value pid, E.Value uname) -> ServerUser pid uname) group
          latestActivity = M.gameLastMoveMadeAt game <|> Just (M.gameCreatedAt game)
      Just $ GameSummaryEntity
        (M.gameGameId game)
        latestActivity
        False
        (M.gameBoard game)
        localisedSetup
        players

withPool = flip runSqlPersistMPool
