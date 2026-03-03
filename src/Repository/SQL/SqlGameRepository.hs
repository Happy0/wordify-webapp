

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
import Repository.GameRepository (GameRepository, GameSummary (GameSummary), UserId, getActiveUserGames, getRecentlyActiveGames)
import Prelude

data GameRepositorySQLBackend = GameRepositorySQLBackend (Pool SqlBackend) (Map.Map T.Text LocalisedGameSetup)

instance GameRepository GameRepositorySQLBackend where
  getActiveUserGames (GameRepositorySQLBackend pool gameSetups) userId = withPool pool (activeUserGames gameSetups userId)
  getRecentlyActiveGames (GameRepositorySQLBackend pool gameSetups) n = withPool pool (recentlyActiveGames gameSetups n)

activeUserGames :: (Monad m, MonadIO m) => Map.Map T.Text LocalisedGameSetup -> UserId -> E.SqlPersistT m [GameSummary]
activeUserGames gameSetups userId = do
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
      return (me, game, usr ^. M.UserUsername, E.just (other ^. M.PlayerPlayerNumber))
  return $ toGameSummaries gameSetups rows

toGameSummaries :: Map.Map T.Text LocalisedGameSetup -> [(E.Entity M.Player, E.Entity M.Game, E.Value (Maybe T.Text), E.Value (Maybe Int))] -> [GameSummary]
toGameSummaries gameSetups rows =
  let grouped = groupBy (\(_, E.Entity _ g1, _, _) (_, E.Entity _ g2, _, _) -> M.gameGameId g1 == M.gameGameId g2) rows
   in mapMaybe groupToSummary grouped
  where
    groupToSummary :: [(E.Entity M.Player, E.Entity M.Game, E.Value (Maybe T.Text), E.Value (Maybe Int))] -> Maybe GameSummary
    groupToSummary [] = Nothing
    groupToSummary group@((E.Entity _ player, E.Entity _ game, _, _):_) =
      if isNothing (M.gameFinishedAt game)
        then do
          let localeCode = fromMaybe "en" (M.gameBagLocale game)
          localisedSetup <- Map.lookup localeCode gameSetups
          let otherNames = concatMap extractName group
              totalPlayers = length otherNames + 1
              playable = isPlayerMove (M.gameCurrentMoveNumber game) totalPlayers (M.playerPlayerNumber player)
          Just $ GameSummary
                (M.gameGameId game)
                (M.gameLastMoveMadeAt game <|> Just (M.gameCreatedAt game))
                playable
                (M.gameBoard game)
                localisedSetup
                otherNames
        else Nothing

    extractName :: (E.Entity M.Player, E.Entity M.Game, E.Value (Maybe T.Text), E.Value (Maybe Int)) -> [Maybe T.Text]
    extractName (_, _, _, E.Value Nothing) = []  -- no other player in this row (LEFT JOIN produced NULL)
    extractName (_, _, E.Value uname, _) = [uname]

    isPlayerMove :: Int -> Int -> Int -> Bool
    isPlayerMove currentMoveNumber numberOfPlayers playerNumber =
      case currentMoveNumber `mod` numberOfPlayers of
        x | x > 0 -> x == playerNumber
        _ -> playerNumber == numberOfPlayers

recentlyActiveGames :: (Monad m, MonadIO m) => Map.Map T.Text LocalisedGameSetup -> Int -> E.SqlPersistT m [GameSummary]
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
        return (game, usr ^. M.UserUsername)
      return $ toGlobalGameSummaries gameSetups rows

toGlobalGameSummaries :: Map.Map T.Text LocalisedGameSetup -> [(E.Entity M.Game, E.Value (Maybe T.Text))] -> [GameSummary]
toGlobalGameSummaries gameSetups rows =
  let grouped = groupBy (\(E.Entity _ g1, _) (E.Entity _ g2, _) -> M.gameGameId g1 == M.gameGameId g2) rows
   in mapMaybe groupToSummary grouped
  where
    groupToSummary [] = Nothing
    groupToSummary group@((E.Entity _ game, _):_) = do
      let localeCode = fromMaybe "en" (M.gameBagLocale game)
      localisedSetup <- Map.lookup localeCode gameSetups
      let playerNames = map (\(_, E.Value uname) -> uname) group
          latestActivity = M.gameLastMoveMadeAt game <|> Just (M.gameCreatedAt game)
      Just $ GameSummary
        (M.gameGameId game)
        latestActivity
        False
        (M.gameBoard game)
        localisedSetup
        playerNames

withPool = flip runSqlPersistMPool
