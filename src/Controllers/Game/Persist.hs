module Controllers.Game.Persist (getGame, getChatMessages, persistNewGame, getLobbyPlayer, persistGameUpdate, persistNewLobby, persistNewLobbyPlayer, deleteLobby, getLobby) where

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
import qualified Wordify.Rules.Player as P
import Wordify.Rules.Pos
import Wordify.Rules.Tile
import Prelude

getChatMessages gameId =
  selectSource [M.ChatMessageGame ==. gameId] [Asc M.ChatMessageCreatedAt]
    $= chatMessageFromEntity

deleteLobby :: App -> Text -> IO ()
deleteLobby app gameId = do
  withPool (appConnPool app) $ do
    deleteWhere [M.LobbyGameId ==. gameId]
    deleteWhere [M.LobbyPlayerGame ==. gameId]

getLobby :: ConnectionPool -> LocalisedGameSetups -> Text -> IO (Either Text GameLobby)
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
        GameSetup dictionary bag <- hoistEither (note "Locale invalid" maybeLocalisedSetup)
        lobbyPlayers <- hoistEither $ makeGameStatePlayers numPlayers
        tiles <- hoistEither $ dbTileRepresentationToTiles bag originalLetterBag
        let bag = makeBagUsingGenerator tiles (read (unpack letterBagSeed) :: StdGen)
        pendingGame <- hoistEither $ mapLeft (pack . show) (makeGame lobbyPlayers bag dictionary)
        numConnections <- liftIO $ newTVarIO 0
        return $ GameLobby pendingGame serverPlayers numPlayers channel playerIdGeneratorTvar createdAt numConnections
    Left err -> return $ Left err

getLobbyPlayer :: ConnectionPool -> Text -> Text -> IO (Maybe ServerPlayer)
getLobbyPlayer pool gameId playerId = do
  playerEntity <- withPool pool $ selectFirst [M.LobbyPlayerGame ==. gameId, M.LobbyPlayerPlayerId ==. playerId] []

  case playerEntity of
    Nothing -> pure Nothing
    Just entity -> Just <$> playerFromLobbyEntity pool entity

addDisplayNames :: [ServerPlayer] -> [P.Player] -> [P.Player]
addDisplayNames serverPlayers gameStatePlayers = Prelude.zipWith addDisplayName serverPlayers gameStatePlayers
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

getGame :: ConnectionPool -> LocalisedGameSetups -> Text -> IO (Either Text ServerGame)
getGame pool localisedGameSetups gameId = do
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
      serverPlayers <- mapM (playerFromEntity pool) playerModels

      runExceptT $ do
        let locale = maybe "en" id maybeLocale
        let maybeLocalisedSetup = Mp.lookup locale localisedGameSetups
        GameSetup dictionary bag <- hoistEither (note "Locale invalid" maybeLocalisedSetup)
        internalPlayers <- hoistEither $ makeGameStatePlayers (L.length playerModels)
        tiles <- hoistEither $ dbTileRepresentationToTiles bag bagText
        let letterBag = makeBagUsingGenerator tiles (read (unpack bagSeed) :: StdGen)
        game <- hoistEither $ mapLeft (pack . show) (makeGame internalPlayers letterBag dictionary)
        let playersWithNamesAdded = addDisplayNames serverPlayers (players game)
        let gameWithDisplayNamesSet = setDisplayNames playersWithNamesAdded game

        currentGame <- hoistEither $ playThroughGame gameWithDisplayNamesSet letterBag moveModels
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
  gamePlayers <- readTVarIO $ (playing serverGame)
  let History letterBag _ = history gameState
  withPool pool $ do
    gameDbId <-
      insert $
        M.Game
          gameId
          (tilesToDbRepresentation (tiles letterBag))
          (pack $ show (getGenerator letterBag))
          (Just locale)

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
        \(playerNumber, (ServerPlayer playerName identifier gameId active lastActive)) ->
          insert $
            M.Player gameId identifier playerNumber lastActive

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

playerFromEntity :: ConnectionPool -> Entity M.Player -> IO ServerPlayer
playerFromEntity pool (Entity _ (M.Player gameId playerId _ lastActive)) =
  do
    user <- getUser pool playerId
    case user of
      -- User was deleted from database?
      Nothing -> return $ makeNewPlayer Nothing playerId gameId False Nothing
      Just (AuthUser ident nickname) -> return (makeNewPlayer nickname gameId playerId False lastActive)

playerFromLobbyEntity :: ConnectionPool -> Entity M.LobbyPlayer -> IO ServerPlayer
playerFromLobbyEntity pool (Entity _ (M.LobbyPlayer gameId playerId _ lastActive)) =
  do
    user <- getUser pool playerId
    case user of
      -- User was deleted from database?
      Nothing -> return $ makeNewPlayer Nothing playerId gameId False Nothing
      Just (AuthUser ident nickname) -> return (makeNewPlayer nickname gameId playerId False lastActive)

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
