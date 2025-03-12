module Controllers.Game.Model.ServerGame
  ( ServerGame,
    ServerGameSnapshot (ServerGameSnapshot),
    makeNewServerGame,
    makeServerGame,
    makeServerGameSnapshot,
    gameId,
    game,
    playing,
    broadcastChannel,
    numConnections,
    increaseConnectionsByOne,
    decreaseConnectionsByOne,
    getServerPlayer,
  )
where

import ClassyPrelude (UTCTime)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Controllers.Common.CacheableSharedResource
import Controllers.Game.Api
import Controllers.Game.Model.ServerPlayer
import qualified Data.List.Safe as SL
import Data.Text
import Wordify.Rules.Game
import Prelude

data ServerGameSnapshot = ServerGameSnapshot
  { id :: Text,
    gameState :: Game,
    players :: [ServerPlayer],
    created :: UTCTime,
    lastMove :: Maybe UTCTime,
    finished :: Maybe UTCTime
  }

data ServerGame = ServerGame
  { gameId :: Text,
    game :: TVar Game,
    playing :: TVar [ServerPlayer],
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
  players <- readTVar playing
  gameState <- readTVar game
  lastMoveMade <- readTVar lastMoveMadeAt
  finished <- readTVar finishedAt
  return $ ServerGameSnapshot id gameState players createdAt lastMoveMade finished

makeNewServerGame :: Text -> Game -> [ServerPlayer] -> UTCTime -> STM ServerGame
makeNewServerGame gameId initialGameState players createdAt = do
  connections <- newTVar 0
  initialPlayerState <- newTVar players
  lastMoveMadeAt <- newTVar Nothing
  finishedAt <- newTVar Nothing
  game <- newTVar initialGameState
  messageChannel <- newBroadcastTChan
  return (ServerGame gameId game initialPlayerState messageChannel connections createdAt lastMoveMadeAt finishedAt)

makeServerGame :: Text -> Game -> [ServerPlayer] -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> STM ServerGame
makeServerGame gameId gameState serverPlayers createdAt lastMoveMadeAt finishedAt = do
  connections <- newTVar 0
  currentPlayerState <- newTVar serverPlayers
  lastMoveMade <- newTVar lastMoveMadeAt
  finished <- newTVar finishedAt
  game <- newTVar gameState
  messageChannel <- newBroadcastTChan
  return (ServerGame gameId game currentPlayerState messageChannel connections createdAt lastMoveMade finished)

updateLastMoveMade :: ServerGame -> UTCTime -> STM ()
updateLastMoveMade serverGame moveTime = undefined

updateGameFinishedAt :: ServerGame -> UTCTime -> STM ()
updateGameFinishedAt serverGame finishTime = undefined

setPlayerActive :: ServerGame -> Text -> UTCTime -> STM ()
setPlayerActive serverGame playerId atTime = undefined

setPlayerInactive :: ServerGame -> Text -> UTCTime -> STM ()
setPlayerInactive serverGame playerId atTime = undefined

updatePlayerActive :: [ServerPlayer] -> Text -> UTCTime -> [ServerPlayer]
updatePlayerActive players activePlayer atTime = undefined

updatePlayerInactive :: [ServerPlayer] -> Text -> UTCTime -> [ServerPlayer]
updatePlayerInactive players inactivePlayer atTime = undefined

getServerPlayer :: ServerGame -> Int -> STM (Maybe ServerPlayer)
getServerPlayer serverGame playerNumber =
  do
    playingx <- readTVar (playing serverGame)
    return (playingx SL.!! (playerNumber - 1))

increaseConnectionsByOne :: ServerGame -> STM ()
increaseConnectionsByOne serverGame = modifyTVar (numConnections serverGame) succ

decreaseConnectionsByOne :: ServerGame -> STM ()
decreaseConnectionsByOne serverGame = modifyTVar (numConnections serverGame) decreaseByOne
  where
    decreaseByOne i = i - 1
