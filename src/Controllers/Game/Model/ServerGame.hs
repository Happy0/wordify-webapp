module Controllers.Game.Model.ServerGame
  ( ServerGame,
    ServerGameSnapshot (ServerGameSnapshot, snapshotGameId, gameState, snapshotPlayers, gameLocalisation, created),
    lastMove,
    getServerPlayer,
    getPlayerNumber,
    makeNewServerGame,
    makeServerGame,
    makeServerGameSnapshot,
    updateGameFinishedAt,
    updateLastMoveMade,
    increasePlayerConnections,
    decreasePlayerConnections,
    gameId,
    game,
    playing,
    broadcastChannel,
    getServerPlayerSnapshot,
    getSnapshotPlayerNumber,
    currentPlayerToMove,
    gameSetup,
    playerIsInGame
  )
where

import ClassyPrelude (UTCTime)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Controllers.Game.GameMessage
import Controllers.Game.Model.ServerPlayer (ServerPlayer (ServerPlayer))
import qualified Controllers.Game.Model.ServerPlayer as SP
import Controllers.User.Model.AuthUser
import qualified Data.List as L
import Data.Maybe
import Data.Maybe (listToMaybe)
import Data.Text
import qualified Wordify.Rules.Game as G
import Prelude
import Model.GameSetup (LocalisedGameSetup)

-- TODO: move this and associated functions to own file
data ServerGameSnapshot = ServerGameSnapshot
  { snapshotGameId :: Text,
    gameState :: G.Game,
    snapshotPlayers :: [SP.ServerPlayer],
    created :: UTCTime,
    snapshotLastMove :: Maybe UTCTime,
    finished :: Maybe UTCTime,
    gameLocalisation :: LocalisedGameSetup
  }

lastMove :: ServerGameSnapshot -> UTCTime
lastMove snapshot = fromMaybe (created snapshot) (snapshotLastMove snapshot)

data ServerGame = ServerGame
  { gameId :: Text,
    game :: TVar G.Game,
    playing :: [(Text, TVar SP.ServerPlayer)],
    broadcastChannel :: TChan GameMessage,
    createdAt :: UTCTime,
    lastMoveMadeAt :: TVar (Maybe UTCTime),
    finishedAt :: TVar (Maybe UTCTime),
    gameSetup :: LocalisedGameSetup
  }

makeServerGameSnapshot :: ServerGame -> STM ServerGameSnapshot
makeServerGameSnapshot (ServerGame id game playing _ createdAt lastMoveMadeAt finishedAt localisation) = do
  players <- mapM (readTVar . snd) playing
  gameState <- readTVar game
  lastMoveMade <- readTVar lastMoveMadeAt
  finished <- readTVar finishedAt
  return $ ServerGameSnapshot id gameState players createdAt lastMoveMade finished localisation

makeNewServerGame :: Text -> G.Game -> [SP.ServerPlayer] -> UTCTime -> LocalisedGameSetup -> STM ServerGame
makeNewServerGame gameId initialGameState players createdAt gameSetup = do
  initialPlayerState <- mapM makeServerPlayerState players
  lastMoveMadeAt <- newTVar Nothing
  finishedAt <- newTVar Nothing
  game <- newTVar initialGameState
  messageChannel <- newBroadcastTChan
  return (ServerGame gameId game initialPlayerState messageChannel createdAt lastMoveMadeAt finishedAt gameSetup)

makeServerGame :: Text -> G.Game -> [SP.ServerPlayer] -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> LocalisedGameSetup -> STM ServerGame
makeServerGame gameId gameState serverPlayers createdAt lastMoveMadeAt finishedAt gameSetup = do
  currentPlayerStates <- mapM makeServerPlayerState serverPlayers
  lastMoveMade <- newTVar lastMoveMadeAt
  finished <- newTVar finishedAt
  game <- newTVar gameState
  messageChannel <- newBroadcastTChan
  return (ServerGame gameId game currentPlayerStates messageChannel createdAt lastMoveMade finished gameSetup)

makeServerPlayerState :: SP.ServerPlayer -> STM (Text, TVar ServerPlayer)
makeServerPlayerState serverPlayer =
  (,) (SP.playerId serverPlayer) <$> newTVar serverPlayer

updateLastMoveMade :: ServerGame -> UTCTime -> STM ()
updateLastMoveMade serverGame moveTime = writeTVar (lastMoveMadeAt serverGame) (Just moveTime)

updateGameFinishedAt :: ServerGame -> UTCTime -> STM ()
updateGameFinishedAt serverGame finishTime = writeTVar (finishedAt serverGame) (Just finishTime)

increasePlayerConnections :: ServerGame -> AuthUser -> UTCTime -> STM (Maybe Int)
increasePlayerConnections serverGame user now = do
  let player = getServerPlayer serverGame user

  case player of
    Nothing -> return Nothing
    Just p -> do
      modifyTVar p (`SP.addConnection` now)
      Just . SP.numConnections <$> readTVar p

decreasePlayerConnections :: ServerGame -> AuthUser -> UTCTime -> STM (Maybe Int)
decreasePlayerConnections serverGame user now = do
  let player = getServerPlayer serverGame user

  case player of
    Nothing -> pure Nothing
    Just p -> do
      modifyTVar p (`SP.removeConnection` now)
      Just . SP.numConnections <$> readTVar p

getServerPlayer :: ServerGame -> AuthUser -> Maybe (TVar SP.ServerPlayer)
getServerPlayer serverGame user = snd <$> L.find (isUser user) (playing serverGame)
  where
    isUser :: AuthUser -> (Text, TVar SP.ServerPlayer) -> Bool
    isUser (AuthUser userId _) (playerId, _) = userId == playerId

getServerPlayerSnapshot :: ServerGameSnapshot -> AuthUser -> Maybe SP.ServerPlayer
getServerPlayerSnapshot gameSnapshot user = L.find (isUser user) (snapshotPlayers gameSnapshot)
  where
    isUser :: AuthUser -> SP.ServerPlayer -> Bool
    isUser (AuthUser userId _) serverPlayer = SP.playerId serverPlayer == userId

getPlayerNumber :: ServerGame -> AuthUser -> Maybe Int
getPlayerNumber serverGame (AuthUser userId _) = do
  let playerIds = Prelude.map fst (playing serverGame)
  fst <$> L.find (\(_, playerId) -> userId == playerId) (Prelude.zip [1 .. 4] playerIds)

getSnapshotPlayerNumber :: ServerGameSnapshot -> AuthUser -> Maybe Int
getSnapshotPlayerNumber serverGame (AuthUser userId _) = do
  let playerIds = Prelude.map SP.playerId (snapshotPlayers serverGame)
  fst <$> L.find (\(_, playerId) -> userId == playerId) (Prelude.zip [1 .. 4] playerIds)

playerIsInGame :: ServerGameSnapshot -> AuthUser -> Bool 
playerIsInGame snapshot = isJust . getServerPlayerSnapshot snapshot

currentPlayerToMove :: ServerGameSnapshot -> Maybe Text
currentPlayerToMove snapshot =
  let pNum = G.playerNumber (gameState snapshot)
      players = snapshotPlayers snapshot
  in SP.playerId <$> listToMaybe (Prelude.drop (pNum - 1) players)