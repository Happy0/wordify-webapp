module Controllers.Game.Persist (getGame, persistNewGame, getLobbyPlayer, persistGameUpdate, persistNewLobby, persistNewLobbyPlayer, deleteLobby, getLobby, updatePlayerLastSeen) where

import ClassyPrelude (Either (Left, Right), IO, Maybe (Just, Nothing), Word64, error, flip, fst, id, liftIO, mapConcurrently, maybe, otherwise, putStrLn, repeat, show, snd, ($), (+), (++), (.), (==), String, Bool, notElem)
import Control.Applicative
import Control.Concurrent.STM
import Control.Error.Util
import Control.Monad
import Control.Monad.Except
import Controllers.Game.Api
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.GameLobby.Model.GameLobby
import Controllers.User.Model.AuthUser
import Controllers.User.Persist
import qualified Data.Char as C
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Map as Mp
import Data.Pool
import qualified Data.Text as T
import Data.Time.Clock
import Database.Persist.Sql
import Foundation
import qualified Model as M
import Repository.GameRepository (GameSummary)
import System.Random
import System.Random.Internal
import System.Random.SplitMix
import Text.Read (read)
import Wordify.Rules.Board
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import qualified Wordify.Rules.Player as P
import Wordify.Rules.Pos
import Wordify.Rules.Tile
import qualified Data.List.Split as SL
import Database.Sqlite (Connection)
import Database.Esqueleto (ConnectionPool)
import Model.GameSetup (LocalisedGameSetup(..))

deleteLobby :: App -> T.Text -> IO ()
deleteLobby app gameId = do
  withPool (appConnPool app) $ do
    deleteWhere [M.LobbyGameId ==. gameId]
    deleteWhere [M.LobbyPlayerGame ==. gameId]

getLobby :: ConnectionPool -> LocalisedGameSetups -> T.Text -> IO (Either T.Text GameLobby)
getLobby pool localisedGameSetups gameId = do
  dbEntries <- withPool pool $ do
    maybeLobby <- selectFirst [M.LobbyGameId ==. gameId] []
    case maybeLobby of
      Nothing -> return $ Left (T.concat ["Game with id ", gameId, " does not exist"])
      Just lobbyModel -> do
        players <- selectList [M.LobbyPlayerGame ==. gameId] []
        return $ Right (lobbyModel, players)

  case dbEntries of
    Right (Entity _ (M.Lobby gameId originalLetterBag letterBagSeed maybeLocale numPlayers createdAt), playerModels) -> do
      players <- sequence $ L.map (playerFromLobbyEntity pool) playerModels
      serverPlayers <- newTVarIO players
      channel <- newBroadcastTChanIO
      playerIdGenerator <- getStdGen
      playerIdGeneratorTvar <- newTVarIO playerIdGenerator
      runExceptT $ do
        let locale = maybe "en" id maybeLocale
        -- This could be more efficient than individual fetches, but it doesn't matter for now
        let maybeLocalisedSetup = Mp.lookup locale localisedGameSetups
        setup@(GameSetup _ dictionary bag _) <- hoistEither (note "Locale invalid" maybeLocalisedSetup)
        lobbyPlayers <- hoistEither $ makeGameStatePlayers numPlayers
        tiles <- hoistEither $ dbTileRepresentationToTiles bag originalLetterBag
        let bag = makeBagUsingGenerator tiles (stdGenFromText letterBagSeed :: StdGen)
        pendingGame <- hoistEither $ mapLeft (T.pack . show) (makeGame lobbyPlayers bag dictionary)
        return $ GameLobby pendingGame serverPlayers numPlayers channel playerIdGeneratorTvar createdAt locale setup
    Left err -> return $ Left err

getLobbyPlayer :: ConnectionPool -> T.Text -> T.Text -> IO (Maybe ServerPlayer)
getLobbyPlayer pool gameId playerId = do
  playerEntity <- withPool pool $ selectFirst [M.LobbyPlayerGame ==. gameId, M.LobbyPlayerPlayerId ==. playerId] []

  case playerEntity of
    Nothing -> pure Nothing
    Just entity -> Just <$> playerFromLobbyEntity pool entity

addDisplayNames :: [ServerPlayer] -> [P.Player] -> [P.Player]
addDisplayNames serverPlayers gameStatePlayers = L.zipWith addDisplayName serverPlayers gameStatePlayers
  where
    -- TODO: expose function to modify name of player in game state in wordify lib
    addDisplayName serverPlayer gameStatePlayer =
      let serverName = maybe (P.name gameStatePlayer) T.unpack (name serverPlayer)
       in let player = P.makePlayer serverName
           in let playerWithTiles = P.giveTiles player (P.tilesOnRack gameStatePlayer)
               in let playerWithScore = P.increaseScore playerWithTiles (P.score gameStatePlayer)
                   in let playerWithEndBonus = P.giveEndWinBonus playerWithScore (P.endBonus playerWithScore)
                       in playerWithEndBonus

setDisplayNames :: [P.Player] -> Game -> Game
setDisplayNames players@[player1, player2] game = game {player1 = player1, player2 = player2, currentPlayer = getCurrentPlayer game players (playerNumber game)}
setDisplayNames players@[player1, player2, player3] game = game {player1 = player1, player2 = player2, optionalPlayers = Just (player3, Nothing), currentPlayer = getCurrentPlayer game players (playerNumber game)}
setDisplayNames players@[player1, player2, player3, player4] game = game {player1 = player1, player2 = player2, optionalPlayers = Just (player3, Just player4), currentPlayer = getCurrentPlayer game players (playerNumber game)}
setDisplayNames _ game = game

getCurrentPlayer game (player1 : x) 1 = player1
getCurrentPlayer game (player1 : player2 : x) 2 = player2
getCurrentPlayer game (player1 : player2 : player3 : x) 3 = player3
getCurrentPlayer game (player1 : player2 : player3 : player4 : x) 4 = player4
getCurrentPlayer game _ _ = currentPlayer game

getGame :: ConnectionPool -> LocalisedGameSetups -> T.Text -> IO (Either T.Text ServerGame)
getGame pool localisedGameSetups gameId = do
  dbEntries <- withPool pool $ do
    maybeGame <- selectFirst [M.GameGameId ==. gameId] []
    case maybeGame of
      Nothing -> return $ Left (T.concat ["Game with id ", gameId, " does not exist"])
      Just gameModel -> do
        players <- selectList [M.PlayerGameId ==. gameId] []
        moves <- selectList [M.MoveGame ==. gameId] []
        return $ Right (gameModel, players, L.map entityVal moves)

  case dbEntries of
    Right (Entity _ (M.Game _ bagText bagSeed maybeLocale gameCreatedAt gameFinishedAt lastMoveMadeAt currentMoveNumber _), playerModels, moveModels) -> do
      -- This could be more efficient than individual fetches, but it doesn't matter for now
      serverPlayers <- mapConcurrently (playerFromEntity pool) playerModels

      runExceptT $ do
        let locale = maybe "en" id maybeLocale
        let maybeLocalisedSetup = Mp.lookup locale localisedGameSetups
        setup@(GameSetup _ dictionary bag extraRules) <- hoistEither (note "Locale invalid" maybeLocalisedSetup)
        internalPlayers <- hoistEither $ makeGameStatePlayers (L.length playerModels)
        tiles <- hoistEither $ dbTileRepresentationToTiles bag bagText
        let letterBag = makeBagUsingGenerator tiles (stdGenFromText bagSeed :: StdGen)
        game <- hoistEither $ mapLeft (T.pack . show) (makeGame internalPlayers letterBag dictionary)
        let playersWithNamesAdded = addDisplayNames serverPlayers (players game)
        let gameWithDisplayNamesSet = setDisplayNames playersWithNamesAdded game

        currentGame <- hoistEither $ playThroughGame gameWithDisplayNamesSet letterBag moveModels

        liftIO $ atomically (makeServerGame gameId currentGame serverPlayers gameCreatedAt lastMoveMadeAt gameFinishedAt setup)
    Left err -> return $ Left err

playThroughGame :: Game -> LetterBag -> [M.Move] -> Either T.Text Game
playThroughGame game initialBag moves = foldM playNextMove game moves
  where
    playNextMove :: Game -> M.Move -> Either T.Text Game
    playNextMove game moveModel =
      case moveFromEntity (board game) initialBag moveModel of
        Left err -> Left err
        Right move ->
          let moveResult = makeMove game move
           in case moveResult of
                Left invalidState -> Left $ T.pack . show $ invalidState
                Right moveResult -> Right (newGame moveResult)

    scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
    scanM f q [] = return [q]
    scanM f q (x : xs) =
      do
        q2 <- f q x
        qs <- scanM f q2 xs
        return (q : qs)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft func (Left err) = Left $ func err
mapLeft _ (Right r) = Right r

persistNewLobby :: Pool SqlBackend -> T.Text -> T.Text -> GameLobby -> IO ()
persistNewLobby pool gameId locale gameLobby = do
  let History letterBag _ = history (pendingGame gameLobby)
  let game = pendingGame gameLobby

  withPool pool $ do
    _ <-
      insert $
        M.Lobby
          gameId
          (tilesToDbRepresentation (tiles letterBag))
          (stdGenToText (getGenerator (bag game)))
          (Just locale)
          (awaiting gameLobby)
          (openedAt gameLobby)

    return ()

persistNewLobbyPlayer :: Pool SqlBackend -> T.Text -> ServerPlayer -> IO ()
persistNewLobbyPlayer pool gameId serverPlayer = withPool pool $ persistLobbyPlayers gameId [serverPlayer]

{-
    Persists the original game state (before the game has begun)
-}
persistNewGame :: Pool SqlBackend -> T.Text -> T.Text -> ServerGame -> IO ()
persistNewGame pool gameId locale serverGame = do
  _ <- persistGameState pool gameId locale serverGame
  return ()

withPool pool = flip runSqlPersistMPool pool

persistGameState :: Pool SqlBackend -> T.Text -> T.Text -> ServerGame -> IO (Key M.Game)
persistGameState pool gameId locale serverGame = do
  ServerGameSnapshot gameId gameState gamePlayers created lastMove finished <- atomically $ makeServerGameSnapshot serverGame
  let History letterBag _ = history gameState
  let currentMove = L.length (movesMade gameState) + 1
  let boardRepresentation = T.pack (L.take 255 (L.repeat ','))
  withPool pool $ do
    gameDbId <-
      insert $
        M.Game
          gameId
          (tilesToDbRepresentation (tiles letterBag))
          (stdGenToText (getGenerator letterBag))
          (Just locale)
          created
          finished
          lastMove
          currentMove
          boardRepresentation

    persistPlayers gameId gamePlayers
    return gameDbId

persistLobbyPlayers gameId players =
  let playersWithNumbers = L.zip [1 .. 4] players
   in flip mapM_ playersWithNumbers $ do
        \(playerNumber, (ServerPlayer playerName identifier gameId active lastActive)) ->
          insert $
            M.LobbyPlayer gameId identifier playerNumber lastActive

persistPlayers gameId players =
  let playersWithNumbers = L.zip [1 .. 4] players
   in flip mapM_ playersWithNumbers $ do
        \(playerNumber, (ServerPlayer _ identifier gameId _ lastActive)) ->
          insert $
            M.Player gameId identifier playerNumber lastActive

updatePlayerLastSeen :: Pool SqlBackend -> T.Text -> T.Text -> UTCTime -> IO ()
updatePlayerLastSeen pool gameId playerId now = do
  withPool pool (updateWhere [M.PlayerGameId ==. gameId, M.PlayerPlayerId ==. playerId] [M.PlayerLastActive =. Just now])
  return ()

persistGameUpdate :: Pool SqlBackend -> T.Text -> Game -> GameMessage -> IO ()
persistGameUpdate pool gameId _ (PlayerChat chatMessage) = pure ()
persistGameUpdate pool gameId gameState (PlayerBoardMove moveNumber placed _ _ _ _) =
  persistBoardMove pool gameId gameState placed
persistGameUpdate pool gameId gameState (PlayerPassMove moveNumber _ _) = persistPassMove pool gameId gameState
persistGameUpdate pool gameId gameState (PlayerExchangeMove moveNumber _ exchanged _) =
  persistExchangeMove pool gameId gameState exchanged
persistGameUpdate pool gameId gameState (GameEnd moveNumber placed moveSummary) = do
  -- TODO: move to own function
  now <- getCurrentTime
  case placed of
    Nothing -> persistPassMove pool gameId gameState
    Just placed -> persistBoardMove pool gameId gameState placed
persistGameUpdate _ _ _ _ = return ()

persistBoardMove :: Pool SqlBackend -> T.Text -> Game -> [(Pos, Tile)] -> IO ()
persistBoardMove pool gameId gameState placed = do
  let move =
        M.Move
          gameId
          moveNumber
          (Just $ tilesToDbRepresentation (L.map snd placedSorted))
          (Just $ xPos min)
          (Just $ yPos min)
          (Just isHorizontal)

  persistMoveUpdate pool gameId gameState move
  where
    moveNumber = L.length (movesMade gameState)
    placedSorted = L.sort placed
    positions = L.map fst placedSorted
    (min, max) = (L.minimum positions, L.maximum positions)
    isHorizontal = yPos min == yPos max

persistPassMove :: Pool SqlBackend -> T.Text -> Game -> IO ()
persistPassMove pool gameId gameState = do
  persistMoveUpdate pool gameId gameState (M.Move gameId moveNumber Nothing Nothing Nothing Nothing)
  where
    moveNumber = L.length (movesMade gameState)

persistExchangeMove :: Pool SqlBackend -> T.Text -> Game -> [Tile] -> IO ()
persistExchangeMove pool gameId gameState tiles = do
  persistMoveUpdate pool gameId gameState (M.Move gameId moveNumber (Just (tilesToDbRepresentation tiles)) Nothing Nothing Nothing)
  where
    moveNumber = L.length (movesMade gameState)

persistMoveUpdate :: Pool SqlBackend -> T.Text -> Game -> M.Move -> IO ()
persistMoveUpdate pool gameId gameState move@(M.Move _ moveNumber _ _ _ _) = do
  withPool pool $ do
    _ <- insert move
    return ()

  updateGameSummary pool gameId gameState

updateGameSummary :: Pool SqlBackend -> T.Text -> Game -> IO ()
updateGameSummary pool gameId gameState = do
  now <- getCurrentTime
  withPool pool $ do
    let finishedAt = if gameFinished gameState then Just now else Nothing
    let boardRepresentation = gameBoardTextRepresentation gameState
    let currentMoveNumber = L.length (movesMade gameState) + 1
    _ <-
      -- TODO: make this a conditional update based on the lastMove timestamp being greater than the previous
      update
        (M.GameKey gameId)
        [ M.GameCurrentMoveNumber =. currentMoveNumber,
          M.GameLastMoveMadeAt =. Just now,
          M.GameFinishedAt =. finishedAt,
          M.GameBoard =. T.pack boardRepresentation
        ]

    return ()
  where
    gameFinished game = gameStatus game == Finished
    gameBoardTextRepresentation = textRepresentation . board

tilesToDbRepresentation :: [Tile] -> T.Text
tilesToDbRepresentation tiles = T.pack (L.intercalate "," (L.map tileToDbRepresentation tiles))

tileToDbRepresentation :: Tile -> String
tileToDbRepresentation (Letter lettr val) = lettr
tileToDbRepresentation (Blank Nothing) = "_"
tileToDbRepresentation (Blank (Just letter)) = L.map C.toLower letter

moveFromEntity :: Board -> LetterBag -> M.Move -> Either T.Text Move
moveFromEntity board letterBag (M.Move gameId moveNumber Nothing Nothing Nothing Nothing) = Right Pass
moveFromEntity board letterBag (M.Move gameId moveNumber (Just tiles) Nothing Nothing Nothing) =
  case dbTileRepresentationToTiles letterBag tiles of
    Left err -> error $ "you've dun goofed... " ++ show err
    Right tiles -> Right $ Exchange tiles
moveFromEntity
  board
  letterBag
  ( M.Move
      gameId
      moveNumber
      (Just tiles)
      (Just startx)
      (Just starty)
      (Just isHorizontal)
    ) =
    do
      let direction = if isHorizontal then Horizontal else Vertical
      let startPos = posAt (startx, starty)

      positions <- case startPos of
        Nothing -> Left $ "u dun goofed. Start position stored for the move is invalid "
        Just pos -> Right $ emptySquaresFrom board pos (T.length tiles) direction
      tiles <- dbTileRepresentationToTiles letterBag tiles
      return $ PlaceTiles (Mp.fromList (L.zip positions tiles))
moveFromEntity _ _ (m@M.Move {}) = error $ "you've dun goofed, see database logs (hopefully) "

playerFromEntity :: ConnectionPool -> Entity M.Player -> IO ServerPlayer
playerFromEntity pool (Entity _ (M.Player gameId playerId _ lastActive)) =
  do
    user <- getUser pool playerId
    case user of
      -- User was deleted from database?
      Nothing -> return $ makeNewPlayer Nothing gameId playerId 0 lastActive
      Just (AuthUser ident nickname) -> return (makeNewPlayer nickname gameId playerId 0 lastActive)

playerFromLobbyEntity :: ConnectionPool -> Entity M.LobbyPlayer -> IO ServerPlayer
playerFromLobbyEntity pool (Entity _ (M.LobbyPlayer gameId playerId _ lastActive)) =
  do
    user <- getUser pool playerId
    case user of
      -- User was deleted from database?
      Nothing -> return $ makeNewPlayer Nothing gameId playerId 0 Nothing
      Just (AuthUser ident nickname) -> return (makeNewPlayer nickname gameId playerId 0 lastActive)

dbTileRepresentationToTiles :: LetterBag -> T.Text -> Either T.Text [Tile]
dbTileRepresentationToTiles letterBag textRepresentation =
  mapM getTile (SL.splitOn "," (T.unpack textRepresentation))
  where
    letterMap = validLetters letterBag
    getTile :: String -> Either T.Text Tile
    getTile stringRepresentation
      | L.all C.isLower stringRepresentation = Right $ Blank (Just $ L.map C.toUpper stringRepresentation)
      | stringRepresentation == "_" = Right $ Blank Nothing
      | otherwise = case Mp.lookup stringRepresentation letterMap of
          Just tile -> Right tile
          _ -> Left $ T.pack $ stringRepresentation ++ " not found in letterbag"

stdGenToText :: StdGen -> T.Text
stdGenToText stdGen =
  let (a, b) = toSeedStdGen stdGen
   in T.concat [T.pack (show a), " ", T.pack (show b)]

stdGenFromText :: T.Text -> StdGen
stdGenFromText stdGenText =
  let [a, b] = T.split (== ' ') stdGenText
   in fromSeedStdGen (read (T.unpack a), (read (T.unpack b)))

toSeedStdGen :: StdGen -> (Word64, Word64)
toSeedStdGen = unseedSMGen . unStdGen

fromSeedStdGen :: (Word64, Word64) -> StdGen
fromSeedStdGen = StdGen . seedSMGen'