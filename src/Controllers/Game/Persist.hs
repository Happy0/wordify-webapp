module Controllers.Game.Persist (withGame, getGame, getChatMessages, persistNewGame, persistGameUpdate, loadLobby, persistNewLobby, persistNewLobbyPlayer, deleteLobby) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error.Util
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
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
import Data.Text
import qualified Data.Text as T
import Data.Time.Clock
import Database.Persist.Sql
import Foundation
import Model (LobbyPlayer (LobbyPlayer))
import qualified Model as M
import System.Random
import Wordify.Rules.Board
import Wordify.Rules.Dictionary
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import Wordify.Rules.Pos
import Wordify.Rules.Tile
import Prelude

withGame ::
  (MonadIO m, MonadThrow m, MonadUnliftIO m) =>
  App ->
  Text ->
  (Either Text ServerGame -> m c) ->
  m c
withGame app gameId withGameAction = runResourceT $ do
  (releaseKey, maybeGame) <- allocate (loadGame app gameId) maybeReleaseGame
  result <- lift $ withGameAction maybeGame
  return result
  where
    -- allocate (loadGame app dictionary letterBag gameId) maybeReleaseGame withGameAction
    -- TODO: Store the locale of the game in the database

    maybeReleaseGame :: Either Text ServerGame -> IO ()
    maybeReleaseGame (Left _) = putStrLn "release err" >> return ()
    maybeReleaseGame (Right game) = do
      connections <- readTVarIO (numConnections game)
      atomically $ do
        decreaseConnectionsByOne game
        connections <- readTVar $ numConnections game

        if connections == 0
          then do
            let gameCache = games app
            modifyTVar gameCache (Mp.delete gameId)
          else return ()

loadLobby :: App -> Text -> IO (Either Text (TVar GameLobby))
loadLobby app gameId = runExceptT $ getLobbyFromCache <|> getLobbyFromDatabase
  where
    getLobbyFromCache = ExceptT (atomically (loadLobbyFromCache app gameId))
    getLobbyFromDatabase = ExceptT (loadLobbyFromDatabase app gameId (gameLobbies app))

loadGame :: App -> Text -> IO (Either Text ServerGame)
loadGame app gameId = runExceptT $ getGameFromCache <|> getGameFromDatabase
  where
    getGameFromCache = ExceptT (atomically (loadGameFromCache app gameId))
    getGameFromDatabase = ExceptT (loadFromDatabase app gameId (games app))

loadLobbyFromCache :: App -> Text -> STM (Either Text (TVar GameLobby))
loadLobbyFromCache app gameId = do
  let gameCache = gameLobbies app
  maybeLobby <- Mp.lookup gameId <$> readTVar gameCache
  -- forM_ maybeLobby increaseConnectionsByOne
  return (note "Lobby does not exist" maybeLobby)

loadLobbyFromDatabase ::
  App ->
  Text ->
  TVar (Mp.Map Text (TVar GameLobby)) ->
  IO (Either Text (TVar GameLobby))
loadLobbyFromDatabase app gameId gameCache =
  do
    eitherGame <- getLobby app gameId
    case eitherGame of
      Left x -> (putStrLn (show x)) >> (return $ Left x)
      Right lobby -> atomically $ runExceptT (cachedLobby <|> gameInsertedToCache)
        where
          cachedLobby = ExceptT (loadLobbyFromCache app gameId)
          gameInsertedToCache = ExceptT $ do
            lobbyTvar <- newTVar lobby
            modifyTVar gameCache $ Mp.insert gameId lobbyTvar
            -- increaseConnectionsByOne serverGame
            return (Right lobbyTvar)

loadGameFromCache :: App -> Text -> STM (Either Text ServerGame)
loadGameFromCache app gameId = do
  let gameCache = games app
  maybeGame <- Mp.lookup gameId <$> readTVar gameCache
  forM_ maybeGame increaseConnectionsByOne
  return (note "Game does not exist" maybeGame)

loadFromDatabase ::
  App ->
  Text ->
  TVar (Mp.Map Text ServerGame) ->
  IO (Either Text ServerGame)
loadFromDatabase app gameId gameCache =
  do
    eitherGame <- getGame app gameId
    case eitherGame of
      Left err -> return $ Left err
      Right serverGame -> atomically $ runExceptT (cachedGame <|> gameInsertedToCache)
        where
          cachedGame = ExceptT (loadGameFromCache app gameId)
          gameInsertedToCache = ExceptT $ do
            modifyTVar gameCache $ Mp.insert gameId serverGame
            increaseConnectionsByOne serverGame
            return (Right serverGame)

getChatMessages gameId =
  selectSource [M.ChatMessageGame ==. gameId] [Asc M.ChatMessageCreatedAt]
    $= chatMessageFromEntity

deleteLobby :: App -> Text -> IO ()
deleteLobby app gameId = do
  withPool (appConnPool app) $ do
    deleteWhere [M.LobbyGameId ==. gameId]
    deleteWhere [M.LobbyPlayerGame ==. gameId]

getLobby :: App -> Text -> IO (Either Text GameLobby)
getLobby app gameId = do
  let pool = appConnPool app
  putStrLn $ show gameId
  dbEntries <- withPool pool $ do
    maybeLobby <- selectFirst [M.LobbyGameId ==. gameId] []
    case maybeLobby of
      Nothing -> return $ Left (T.concat ["Game with id ", gameId, " does not exist"])
      Just lobbyModel -> do
        players <- selectList [M.LobbyPlayerGame ==. gameId] []
        return $ Right (lobbyModel, players)

  case dbEntries of
    Right (Entity _ (M.Lobby gameId originalLetterBag letterBagSeed maybeLocale numPlayers createdAt), playerModels) -> do
      serverPlayers <- sequence $ L.map (playerFromLobbyEntity app) playerModels
      channel <- newBroadcastTChanIO
      playerIdGenerator <- getStdGen
      playerIdGeneratorTvar <- newTVarIO playerIdGenerator
      runExceptT $ do
        let locale = maybe "en" id maybeLocale
        -- This could be more efficient than individual fetches, but it doesn't matter for now
        let maybeLocalisedSetup = Mp.lookup locale (localisedGameSetups app)
        GameSetup dictionary bag <- hoistEither (note "Locale invalid" maybeLocalisedSetup)
        lobbyPlayers <- hoistEither $ makeGameStatePlayers numPlayers
        tiles <- hoistEither $ dbTileRepresentationToTiles bag originalLetterBag
        let bag = makeBagUsingGenerator tiles (read (unpack letterBagSeed) :: StdGen)
        pendingGame <- hoistEither $ mapLeft (pack . show) (makeGame lobbyPlayers bag dictionary)
        return $ GameLobby pendingGame serverPlayers numPlayers channel playerIdGeneratorTvar createdAt
    Left err -> return $ Left err

getGame :: App -> Text -> IO (Either Text ServerGame)
getGame app gameId = do
  let pool = appConnPool app
  dbEntries <- withPool pool $ do
    maybeGame <- selectFirst [M.GameGameId ==. gameId] []
    case maybeGame of
      Nothing -> return $ Left (T.concat ["Game with id ", gameId, " does not exist"])
      Just gameModel -> do
        players <- selectList [M.PlayerGame ==. gameId] []
        moves <- selectList [M.MoveGame ==. gameId] []
        return $ Right (gameModel, players, L.map entityVal moves)

  case dbEntries of
    Right (Entity _ (M.Game _ bagText bagSeed maybeLocale), playerModels, moveModels) -> do
      -- This could be more efficient than individual fetches, but it doesn't matter for now
      serverPlayers <- sequence $ L.map (playerFromEntity app) playerModels

      runExceptT $ do
        let locale = maybe "en" id maybeLocale
        let maybeLocalisedSetup = Mp.lookup locale (localisedGameSetups app)
        GameSetup dictionary bag <- hoistEither (note "Locale invalid" maybeLocalisedSetup)
        internalPlayers <- hoistEither $ makeGameStatePlayers (L.length playerModels)
        tiles <- hoistEither $ dbTileRepresentationToTiles bag bagText
        let letterBag = makeBagUsingGenerator tiles (read (unpack bagSeed) :: StdGen)
        game <- hoistEither $ mapLeft (pack . show) (makeGame internalPlayers letterBag dictionary)

        currentGame <- hoistEither $ playThroughGame game letterBag moveModels
        currentGameVar <- liftIO $ newTVarIO currentGame

        channel <- liftIO $ newBroadcastTChanIO
        liftIO $ atomically (makeServerGame gameId currentGameVar serverPlayers channel)
    Left err -> return $ Left err

playThroughGame :: Game -> LetterBag -> [M.Move] -> Either Text Game
playThroughGame game initialBag moves = foldM playNextMove game moves
  where
    playNextMove :: Game -> M.Move -> Either Text Game
    playNextMove game moveModel =
      case moveFromEntity (board game) initialBag moveModel of
        Left err -> Left err
        Right move ->
          let moveResult = makeMove game move
           in case moveResult of
                Left invalidState -> Left $ pack . show $ invalidState
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

persistNewLobby :: Pool SqlBackend -> Text -> Text -> GameLobby -> IO ()
persistNewLobby pool gameId locale gameLobby = do
  let game = pendingGame gameLobby

  withPool pool $ do
    _ <-
      insert $
        M.Lobby
          gameId
          (tilesToDbRepresentation (tiles (bag game)))
          (pack $ show (getGenerator (bag game)))
          (Just locale)
          (awaiting gameLobby)
          (openedAt gameLobby)

    return ()

persistNewLobbyPlayer :: Pool SqlBackend -> Text -> ServerPlayer -> IO ()
persistNewLobbyPlayer pool gameId serverPlayer = withPool pool $ persistLobbyPlayers gameId [serverPlayer]

{-
    Persists the original game state (before the game has begun)
-}
persistNewGame :: Pool SqlBackend -> Text -> Text -> ServerGame -> IO ()
persistNewGame pool gameId locale serverGame = do
  _ <- persistGameState pool gameId locale serverGame
  return ()

withPool pool = flip runSqlPersistMPool pool

persistGameState :: Pool SqlBackend -> Text -> Text -> ServerGame -> IO (Key M.Game)
persistGameState pool gameId locale serverGame = do
  gameState <- readTVarIO (game serverGame)
  let History letterBag _ = history gameState
  withPool pool $ do
    gameDbId <-
      insert $
        M.Game
          gameId
          (tilesToDbRepresentation (tiles letterBag))
          (pack $ show (getGenerator letterBag))
          (Just locale)

    persistPlayers gameId (playing serverGame)
    return gameDbId

persistLobbyPlayers gameId players =
  let playersWithNumbers = L.zip [1 .. 4] players
   in flip mapM_ playersWithNumbers $ do
        \(playerNumber, (ServerPlayer playerName identifier)) ->
          insert $
            M.LobbyPlayer gameId identifier playerNumber

persistPlayers gameId players =
  let playersWithNumbers = L.zip [1 .. 4] players
   in flip mapM_ playersWithNumbers $ do
        \(playerNumber, (ServerPlayer playerName identifier)) ->
          insert $
            M.Player gameId identifier playerNumber

persistGameUpdate :: Pool SqlBackend -> Text -> GameMessage -> IO ()
persistGameUpdate pool gameId (PlayerChat chatMessage) = persistChatMessage pool gameId chatMessage
persistGameUpdate pool gameId (PlayerBoardMove moveNumber placed _ _ _ _) =
  persistBoardMove pool gameId moveNumber placed
persistGameUpdate pool gameId (PlayerPassMove moveNumber _ _) = persistPassMove pool gameId moveNumber
persistGameUpdate pool gameId (PlayerExchangeMove moveNumber _ exchanged _) =
  persistExchangeMove pool gameId moveNumber exchanged
persistGameUpdate pool gameId (GameEnd moveNumber placed moveSummary) =
  case placed of
    Nothing -> persistPassMove pool gameId moveNumber
    Just placed -> persistBoardMove pool gameId moveNumber placed
persistGameUpdate _ _ _ = return ()

persistBoardMove :: Pool SqlBackend -> Text -> Int -> [(Pos, Tile)] -> IO ()
persistBoardMove pool gameId moveNumber placed = do
  let move =
        M.Move
          gameId
          moveNumber
          (Just $ tilesToDbRepresentation (Prelude.map snd placedSorted))
          (Just $ xPos min)
          (Just $ yPos min)
          (Just isHorizontal)

  withPool pool $ do
    insert move
    return ()
  where
    placedSorted = L.sort placed
    positions = L.map fst placedSorted
    (min, max) = (L.minimum positions, L.maximum positions)
    isHorizontal = (yPos min == yPos max)

persistPassMove :: Pool SqlBackend -> Text -> Int -> IO ()
persistPassMove pool gameId moveNumber =
  withPool pool $ do
    insert (M.Move gameId moveNumber Nothing Nothing Nothing Nothing)
    return ()

persistExchangeMove :: Pool SqlBackend -> Text -> Int -> [Tile] -> IO ()
persistExchangeMove pool gameId moveNumber tiles =
  withPool pool $ do
    insert (M.Move gameId moveNumber (Just (tilesToDbRepresentation tiles)) Nothing Nothing Nothing)
    return ()

persistChatMessage :: Pool SqlBackend -> Text -> ChatMessage -> IO ()
persistChatMessage pool gameId (ChatMessage user message) = do
  time <- getCurrentTime
  withPool pool $ do
    insert (M.ChatMessage gameId time user message)
    return ()

tilesToDbRepresentation :: [Tile] -> Text
tilesToDbRepresentation tiles = pack $ Prelude.map tileToDbRepresentation tiles

tileToDbRepresentation :: Tile -> Char
tileToDbRepresentation (Letter lettr val) = lettr
tileToDbRepresentation (Blank Nothing) = '_'
tileToDbRepresentation (Blank (Just letter)) = C.toLower letter

chatMessageFromEntity :: Monad m => Conduit (Entity M.ChatMessage) m GameMessage
chatMessageFromEntity = CL.map fromEntity
  where
    fromEntity (Entity _ (M.ChatMessage _ created user message)) = PlayerChat (ChatMessage user message)

moveFromEntity :: Board -> LetterBag -> M.Move -> Either Text Move
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

playerFromEntity :: App -> Entity M.Player -> IO ServerPlayer
playerFromEntity app (Entity _ (M.Player gameId playerId _)) =
  do
    let pool = appConnPool app
    user <- getUser pool playerId
    case user of
      -- User was deleted from database?
      Nothing -> return $ makeNewPlayer Nothing playerId
      Just (AuthUser ident nickname) -> return (makeNewPlayer (nickname) playerId)

playerFromLobbyEntity :: App -> Entity M.LobbyPlayer -> IO ServerPlayer
playerFromLobbyEntity app (Entity _ (M.LobbyPlayer gameId playerId _)) =
  do
    let pool = appConnPool app
    user <- getUser pool playerId
    case user of
      -- User was deleted from database?
      Nothing -> return $ makeNewPlayer Nothing playerId
      Just (AuthUser ident nickname) -> return (makeNewPlayer (nickname) playerId)

dbTileRepresentationToTiles :: LetterBag -> Text -> Either Text [Tile]
dbTileRepresentationToTiles letterBag textRepresentation =
  sequence $ fmap getTile (unpack textRepresentation)
  where
    letterMap = validLetters letterBag
    getTile :: Char -> Either Text Tile
    getTile character
      | (C.isLower character) = Right $ Blank (Just $ C.toUpper character)
      | character == '_' = Right $ Blank Nothing
      | otherwise = case Mp.lookup character letterMap of
        Just tile -> Right tile
        _ -> Left $ pack $ (show character) ++ " not found in letterbag"
