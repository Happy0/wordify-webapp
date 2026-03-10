

module Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend), GameRepository (getActiveUserGames, getRecentlyActiveGames, getGame, startNewGame, saveBoardMove, savePassMove, saveExchangeMove, saveGameEnd, updatePlayerLastSeen)) where

import Control.Applicative ((<|>))
import Control.Error.Util (note, hoistEither)
import Control.Monad (foldM, forM_)
import Control.Monad.Except (runExcept, ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.List (groupBy)
import qualified Data.List as L
import qualified Data.List.Split as SL
import qualified Data.Char as C
import qualified Data.Map as Map
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Data.Word (Word64)
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import qualified Model as M
import Model.GameSetup (LocalisedGameSetup (GameSetup), gameLanguageShortCode)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Repository.GameRepository (GameRepository, GameSummaryEntity (GameSummaryEntity), GameEntity (GameEntity), UserId, getActiveUserGames, getRecentlyActiveGames, getGame, startNewGame, saveBoardMove, savePassMove, saveExchangeMove, saveGameEnd, updatePlayerLastSeen)
import Controllers.User.Model.ServerUser (ServerUser (ServerUser))
import qualified Controllers.User.Model.ServerUser as SU
import Controllers.Game.Model.ServerPlayer (makeGameStatePlayers)

import System.Random (StdGen)
import System.Random.Internal (StdGen (StdGen), unStdGen)
import System.Random.SplitMix (seedSMGen', unseedSMGen)
import Text.Read (read)
import Wordify.Rules.Board
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import qualified Wordify.Rules.Player as P
import Wordify.Rules.Pos
import Wordify.Rules.Tile
import Prelude

data GameRepositorySQLBackend = GameRepositorySQLBackend (Pool SqlBackend) (Map.Map T.Text LocalisedGameSetup)

instance GameRepository GameRepositorySQLBackend where
  getActiveUserGames (GameRepositorySQLBackend pool gameSetups) userId = withPool pool (activeUserGames gameSetups userId)
  getRecentlyActiveGames (GameRepositorySQLBackend pool gameSetups) n = withPool pool (recentlyActiveGames gameSetups n)
  getGame (GameRepositorySQLBackend pool gameSetups) gameId = withPool pool (fetchGameEntity gameSetups gameId)
  startNewGame (GameRepositorySQLBackend pool _) entity = persistNewGame pool entity
  saveBoardMove (GameRepositorySQLBackend pool _) = sqlSaveBoardMove pool
  savePassMove (GameRepositorySQLBackend pool _) = sqlSavePassMove pool
  saveExchangeMove (GameRepositorySQLBackend pool _) = sqlSaveExchangeMove pool
  saveGameEnd (GameRepositorySQLBackend pool _) = sqlSaveGameEnd pool
  updatePlayerLastSeen (GameRepositorySQLBackend pool _) gameId (ServerUser uid _) now =
    withPool pool $ updateWhere [M.PlayerGameId ==. gameId, M.PlayerPlayerId ==. uid] [M.PlayerLastActive =. Just now]

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

persistNewGame :: Pool SqlBackend -> GameEntity -> IO ()
persistNewGame pool (GameEntity gameId game players createdAt lastMoveMadeAt finishedAt setup) = do
  let locale = gameLanguageShortCode setup
      History letterBag _ = history game
      currentMove = L.length (movesMade game) + 1
      boardRepresentation = T.pack (L.replicate 255 ',')
  withPool pool $ do
    _ <- insert $
      M.Game
        gameId
        (tilesToDbRepresentation (tiles letterBag))
        (stdGenToText (getGenerator letterBag))
        (Just locale)
        createdAt
        finishedAt
        lastMoveMadeAt
        currentMove
        boardRepresentation
    forM_ (L.zip [1 :: Int ..] players) $ \(playerNum, (ServerUser uid _, _)) ->
      insert $ M.Player gameId uid playerNum Nothing

sqlSaveBoardMove :: Pool SqlBackend -> T.Text -> Int -> [(Pos, Tile)] -> T.Text -> IO ()
sqlSaveBoardMove pool gameId moveNumber placed boardText = do
  let placedSorted = L.sort placed
      positions = L.map fst placedSorted
      (minPos, maxPos) = (L.minimum positions, L.maximum positions)
      isHorizontal = yPos minPos == yPos maxPos
  withPool pool $ do
    _ <- insert $ M.Move (M.GameKey gameId) moveNumber
           (Just $ tilesToDbRepresentation (L.map snd placedSorted))
           (Just $ xPos minPos) (Just $ yPos minPos) (Just isHorizontal)
    return ()
  updateGameSummary pool gameId moveNumber boardText False

sqlSavePassMove :: Pool SqlBackend -> T.Text -> Int -> T.Text -> IO ()
sqlSavePassMove pool gameId moveNumber boardText = do
  withPool pool $ do
    _ <- insert $ M.Move (M.GameKey gameId) moveNumber Nothing Nothing Nothing Nothing
    return ()
  updateGameSummary pool gameId moveNumber boardText False

sqlSaveExchangeMove :: Pool SqlBackend -> T.Text -> Int -> [Tile] -> T.Text -> IO ()
sqlSaveExchangeMove pool gameId moveNumber tiles boardText = do
  withPool pool $ do
    _ <- insert $ M.Move (M.GameKey gameId) moveNumber
           (Just $ tilesToDbRepresentation tiles) Nothing Nothing Nothing
    return ()
  updateGameSummary pool gameId moveNumber boardText False

sqlSaveGameEnd :: Pool SqlBackend -> T.Text -> Int -> Maybe [(Pos, Tile)] -> T.Text -> IO ()
sqlSaveGameEnd pool gameId moveNumber maybePlaced boardText = do
  withPool pool $ do
    _ <- case maybePlaced of
           Just placed ->
             let placedSorted = L.sort placed
                 positions = L.map fst placedSorted
                 (minPos, maxPos) = (L.minimum positions, L.maximum positions)
                 isHorizontal = yPos minPos == yPos maxPos
             in insert $ M.Move (M.GameKey gameId) moveNumber
                  (Just $ tilesToDbRepresentation (L.map snd placedSorted))
                  (Just $ xPos minPos) (Just $ yPos minPos) (Just isHorizontal)
           Nothing ->
             insert $ M.Move (M.GameKey gameId) moveNumber Nothing Nothing Nothing Nothing
    return ()
  updateGameSummary pool gameId moveNumber boardText True

updateGameSummary :: Pool SqlBackend -> T.Text -> Int -> T.Text -> Bool -> IO ()
updateGameSummary pool gameId moveNumber boardText isFinished = do
  now <- getCurrentTime
  withPool pool $ do
    let finishedAt = if isFinished then Just now else Nothing
    _ <- update (M.GameKey gameId)
           [ M.GameCurrentMoveNumber =. (moveNumber + 1)
           , M.GameLastMoveMadeAt =. Just now
           , M.GameFinishedAt =. finishedAt
           , M.GameBoard =. boardText
           ]
    return ()

tilesToDbRepresentation :: [Tile] -> T.Text
tilesToDbRepresentation ts = T.pack (L.intercalate "," (L.map tileToDbRepresentation ts))

tileToDbRepresentation :: Tile -> String
tileToDbRepresentation (Letter lettr _) = lettr
tileToDbRepresentation (Blank Nothing) = "_"
tileToDbRepresentation (Blank (Just letter)) = L.map C.toLower letter

stdGenToText :: StdGen -> T.Text
stdGenToText stdGen =
  let (a, b) = toSeedStdGen stdGen
   in T.concat [T.pack (show a), " ", T.pack (show b)]

toSeedStdGen :: StdGen -> (Word64, Word64)
toSeedStdGen = unseedSMGen . unStdGen

withPool = flip runSqlPersistMPool

fetchGameEntity :: (Monad m, MonadIO m) => Map.Map T.Text LocalisedGameSetup -> T.Text -> E.SqlPersistT m (Either T.Text GameEntity)
fetchGameEntity gameSetups gameId = do
  maybeGame <- selectFirst [M.GameGameId ==. gameId] []
  case maybeGame of
    Nothing -> return $ Left (T.concat ["Game with id ", gameId, " does not exist"])
    Just (Entity _ game) -> do
      playerRows <- E.select $ E.from $ \(player `E.LeftOuterJoin` usr) -> do
        E.on (usr ^. M.UserIdent E.==. player ^. M.PlayerPlayerId)
        E.where_ (player ^. M.PlayerGameId E.==. E.val gameId)
        E.orderBy [E.asc (player ^. M.PlayerPlayerNumber)]
        return (player ^. M.PlayerPlayerId, usr ^. M.UserUsername, player ^. M.PlayerLastActive)
      moves <- selectList [M.MoveGame ==. M.GameKey gameId] []
      let serverUsers = map (\(E.Value pid, E.Value uname, E.Value lastActive) -> (ServerUser pid uname, lastActive)) playerRows
      return $ buildGameEntity gameSetups gameId game serverUsers (map entityVal moves)

buildGameEntity :: Map.Map T.Text LocalisedGameSetup -> T.Text -> M.Game -> [(ServerUser, Maybe UTCTime)] -> [M.Move] -> Either T.Text GameEntity
buildGameEntity gameSetups gameId game serverUsers moves = runExcept $ do
  let M.Game _ bagText bagSeed maybeLocale gameCreatedAt gameFinishedAt lastMoveMadeAt _ _ = game
      locale = fromMaybe "en" maybeLocale
  setup@(GameSetup _ dictionary bag _ _) <- hoistEither (note "Locale invalid" (Map.lookup locale gameSetups))
  internalPlayers <- hoistEither $ makeGameStatePlayers (L.length serverUsers)
  tiles' <- hoistEither $ dbTileRepresentationToTiles bag bagText
  let letterBag = makeBagUsingGenerator tiles' (stdGenFromText bagSeed :: StdGen)
  initialGame <- hoistEither $ gameMapLeft (T.pack . show) (makeGame internalPlayers letterBag dictionary)
  let playersWithNames = addDisplayNamesFromUsers (map fst serverUsers) (players initialGame)
      gameWithNames = setDisplayNames playersWithNames initialGame
  currentGame <- hoistEither $ playThroughGame gameWithNames letterBag moves
  return $ GameEntity gameId currentGame serverUsers gameCreatedAt lastMoveMadeAt gameFinishedAt setup

addDisplayNamesFromUsers :: [ServerUser] -> [P.Player] -> [P.Player]
addDisplayNamesFromUsers = L.zipWith addDisplayName
  where
    addDisplayName serverUser gameStatePlayer =
      let serverName = maybe (P.name gameStatePlayer) T.unpack (SU.username serverUser)
          player = P.makePlayer serverName
          playerWithTiles = P.giveTiles player (P.tilesOnRack gameStatePlayer)
          playerWithScore = P.increaseScore playerWithTiles (P.score gameStatePlayer)
          playerWithEndBonus = P.giveEndWinBonus playerWithScore (P.endBonus playerWithScore)
      in playerWithEndBonus

setDisplayNames :: [P.Player] -> Game -> Game
setDisplayNames players'@[player1, player2] game = game {player1 = player1, player2 = player2, currentPlayer = getCurrentPlayer game players' (playerNumber game)}
setDisplayNames players'@[player1, player2, player3] game = game {player1 = player1, player2 = player2, optionalPlayers = Just (player3, Nothing), currentPlayer = getCurrentPlayer game players' (playerNumber game)}
setDisplayNames players'@[player1, player2, player3, player4] game = game {player1 = player1, player2 = player2, optionalPlayers = Just (player3, Just player4), currentPlayer = getCurrentPlayer game players' (playerNumber game)}
setDisplayNames _ game = game

getCurrentPlayer :: Game -> [P.Player] -> Int -> P.Player
getCurrentPlayer game (player1 : _) 1 = player1
getCurrentPlayer game (_ : player2 : _) 2 = player2
getCurrentPlayer game (_ : _ : player3 : _) 3 = player3
getCurrentPlayer game (_ : _ : _ : player4 : _) 4 = player4
getCurrentPlayer game _ _ = currentPlayer game

playThroughGame :: Game -> LetterBag -> [M.Move] -> Either T.Text Game
playThroughGame game initialBag = foldM playNextMove game
  where
    playNextMove :: Game -> M.Move -> Either T.Text Game
    playNextMove g moveModel =
      case moveFromEntity (board g) initialBag moveModel of
        Left err -> Left err
        Right move ->
          case makeMove g move of
            Left invalidState -> Left $ T.pack . show $ invalidState
            Right moveResult -> Right (newGame moveResult)

moveFromEntity :: Board -> LetterBag -> M.Move -> Either T.Text Move
moveFromEntity _ _ (M.Move _ _ Nothing Nothing Nothing Nothing) = Right Pass
moveFromEntity _ letterBag (M.Move _ _ (Just tileStr) Nothing Nothing Nothing) =
  case dbTileRepresentationToTiles letterBag tileStr of
    Left err -> Left err
    Right tiles' -> Right $ Exchange tiles'
moveFromEntity board letterBag (M.Move _ _ (Just tileStr) (Just startx) (Just starty) (Just isHorizontal)) = do
  let direction = if isHorizontal then Horizontal else Vertical
  startPos <- case posAt (startx, starty) of
    Nothing -> Left "Invalid start position stored for move"
    Just pos -> Right pos
  positions <- Right $ emptySquaresFrom board startPos (T.length tileStr) direction
  tiles' <- dbTileRepresentationToTiles letterBag tileStr
  return $ PlaceTiles (Map.fromList (L.zip positions tiles'))
moveFromEntity _ _ _ = Left "Unrecognised move format in database"

dbTileRepresentationToTiles :: LetterBag -> T.Text -> Either T.Text [Tile]
dbTileRepresentationToTiles letterBag textRep =
  mapM getTile (SL.splitOn "," (T.unpack textRep))
  where
    letterMap = validLetters letterBag
    getTile :: String -> Either T.Text Tile
    getTile str
      | L.all C.isLower str = Right $ Blank (Just $ L.map C.toUpper str)
      | str == "_" = Right $ Blank Nothing
      | otherwise = case Map.lookup str letterMap of
          Just tile -> Right tile
          _ -> Left $ T.pack $ str ++ " not found in letterbag"

stdGenFromText :: T.Text -> StdGen
stdGenFromText stdGenText =
  let [a, b] = T.split (== ' ') stdGenText
   in fromSeedStdGen (read (T.unpack a), read (T.unpack b))

fromSeedStdGen :: (Word64, Word64) -> StdGen
fromSeedStdGen = StdGen . seedSMGen'

gameMapLeft :: (a -> c) -> Either a b -> Either c b
gameMapLeft func (Left err) = Left $ func err
gameMapLeft _ (Right r) = Right r
