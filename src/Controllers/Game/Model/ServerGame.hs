module Controllers.Game.Model.ServerGame
  ( ServerGame,
    makeServerGame,
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

data ServerGame = ServerGame
  { gameId :: Text,
    game :: TVar Game,
    playing :: TVar [ServerPlayer],
    broadcastChannel :: (TChan GameMessage),
    numConnections :: TVar Int
  }

instance CacheableSharedResource ServerGame where
  decreaseSharersByOne serverGame = decreaseConnectionsByOne serverGame >> readTVar (numConnections serverGame)
  increaseSharersByOne serverGame = increaseConnectionsByOne serverGame >> readTVar (numConnections serverGame)
  numberOfSharers serverGame = readTVar (numConnections serverGame)

makeServerGame :: Text -> TVar Game -> [ServerPlayer] -> (TChan GameMessage) -> STM ServerGame
makeServerGame gameId game players messageChannel = do
  connections <- newTVar 0
  initialPlayerState <- newTVar players
  return (ServerGame gameId game initialPlayerState messageChannel connections)

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
