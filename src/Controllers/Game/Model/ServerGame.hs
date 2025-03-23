module Controllers.Game.Model.ServerGame
  ( ServerGame,
    ServerGameSnapshot (ServerGameSnapshot, snapshotGameId, gameState),
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
    numConnections,
    increaseConnectionsByOne,
    decreaseConnectionsByOne,
  )
where

import ClassyPrelude (UTCTime, whenM)
import ClassyPrelude.Yesod (Value (Bool))
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad (when)
import Control.Monad.STM
import Controllers.Common.CacheableSharedResource
import Controllers.Game.Api
import Controllers.Game.Model.ServerPlayer (ServerPlayer (ServerPlayer))
import qualified Controllers.Game.Model.ServerPlayer as SP
import Data.Conduit.Combinators (iterM)
import qualified Data.List as L
import Data.Maybe
import Data.Text
import GHC.Conc (TVar (TVar))
import Model (User (User))
import Wordify.Rules.Game
import Prelude

data ServerGameSnapshot = ServerGameSnapshot
  { snapshotGameId :: Text,
    gameState :: Game,
    players :: [SP.ServerPlayer],
    created :: UTCTime,
    lastMove :: Maybe UTCTime,
    finished :: Maybe UTCTime
  }

data ServerGame = ServerGame
  { gameId :: Text,
    game :: TVar Game,
    playing :: [(Text, TVar SP.ServerPlayer)],
    broadcastChannel :: (TChan GameMessage),
    numConnections :: TVar Int,
    createdAt :: UTCTime,
    lastMoveMadeAt :: TVar (Maybe UTCTime),
    finishedAt :: TVar (Maybe UTCTime)
  }

instance CacheableSharedResource ServerGame where
  decreaseSharersByOne serverGame = decreaseConnectionsByOne serverGame >> readTVar (numConnections serverGame)
  increaseSharersByOne serverGame = increaseConnectionsByOne serverGame >> readTVar (numConnections serverGame)
  numberOfSharers serverGame = readTVar (numConnections serverGame)

makeServerGameSnapshot :: ServerGame -> STM ServerGameSnapshot
makeServerGameSnapshot (ServerGame id game playing _ _ createdAt lastMoveMadeAt finishedAt) = do
  players <- mapM (readTVar . snd) playing
  gameState <- readTVar game
  lastMoveMade <- readTVar lastMoveMadeAt
  finished <- readTVar finishedAt
  return $ ServerGameSnapshot id gameState players createdAt lastMoveMade finished

makeNewServerGame :: Text -> Game -> [SP.ServerPlayer] -> UTCTime -> STM ServerGame
makeNewServerGame gameId initialGameState players createdAt = do
  connections <- newTVar 0
  initialPlayerState <- mapM makeServerPlayerState players
  lastMoveMadeAt <- newTVar Nothing
  finishedAt <- newTVar Nothing
  game <- newTVar initialGameState
  messageChannel <- newBroadcastTChan
  return (ServerGame gameId game initialPlayerState messageChannel connections createdAt lastMoveMadeAt finishedAt)

makeServerGame :: Text -> Game -> [SP.ServerPlayer] -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> STM ServerGame
makeServerGame gameId gameState serverPlayers createdAt lastMoveMadeAt finishedAt = do
  connections <- newTVar 0
  currentPlayerStates <- mapM makeServerPlayerState serverPlayers
  lastMoveMade <- newTVar lastMoveMadeAt
  finished <- newTVar finishedAt
  game <- newTVar gameState
  messageChannel <- newBroadcastTChan
  return (ServerGame gameId game currentPlayerStates messageChannel connections createdAt lastMoveMade finished)

makeServerPlayerState :: SP.ServerPlayer -> STM (Text, TVar ServerPlayer)
makeServerPlayerState serverPlayer@(ServerPlayer _ playerId _ _ _) =
  (,) playerId <$> newTVar serverPlayer

updateLastMoveMade :: ServerGame -> UTCTime -> STM ()
updateLastMoveMade serverGame moveTime = writeTVar (lastMoveMadeAt serverGame) (Just moveTime)

updateGameFinishedAt :: ServerGame -> UTCTime -> STM ()
updateGameFinishedAt serverGame finishTime = writeTVar (finishedAt serverGame) (Just finishTime)

increasePlayerConnections :: ServerGame -> User -> UTCTime -> STM (Maybe Int)
increasePlayerConnections serverGame user now = do
  let player = getServerPlayer serverGame user

  case player of
    Nothing -> return Nothing
    Just p -> do
      modifyTVar p (`SP.addConnection` now)
      Just . SP.numConnections <$> readTVar p

decreasePlayerConnections :: ServerGame -> User -> UTCTime -> STM (Maybe Int)
decreasePlayerConnections serverGame user now = do
  let player = getServerPlayer serverGame user

  case player of
    Nothing -> pure Nothing
    Just p -> do
      modifyTVar p (`SP.removeConnection` now)
      Just . SP.numConnections <$> readTVar p

getServerPlayer :: ServerGame -> User -> Maybe (TVar SP.ServerPlayer)
getServerPlayer serverGame user = snd <$> L.find (isUser user) (playing serverGame)
  where
    isUser :: User -> (Text, TVar SP.ServerPlayer) -> Bool
    isUser (User userId _) (playerId, playerTvar) = userId == playerId

increaseConnectionsByOne :: ServerGame -> STM ()
increaseConnectionsByOne serverGame = modifyTVar (numConnections serverGame) succ

decreaseConnectionsByOne :: ServerGame -> STM ()
decreaseConnectionsByOne serverGame = modifyTVar (numConnections serverGame) decreaseByOne
  where
    decreaseByOne i = i - 1
