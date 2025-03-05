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
    playing :: [ServerPlayer],
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
  return (ServerGame gameId game players messageChannel connections)

getServerPlayer :: ServerGame -> Int -> Maybe ServerPlayer
getServerPlayer serverGame playerNumber = playing serverGame SL.!! (playerNumber - 1)

increaseConnectionsByOne :: ServerGame -> STM ()
increaseConnectionsByOne serverGame = modifyTVar (numConnections serverGame) succ

decreaseConnectionsByOne :: ServerGame -> STM ()
decreaseConnectionsByOne serverGame = modifyTVar (numConnections serverGame) decreaseByOne
  where
    decreaseByOne i = i - 1
