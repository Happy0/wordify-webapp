module Controllers.Game.Model.ServerGame (ServerGame,
                                          makeServerGame,
                                          gameId,
                                          game,
                                          playing,
                                          broadcastChannel,
                                          numConnections,
                                          increaseConnectionsByOne,
                                          decreaseConnectionsByOne,
                                          getServerPlayer) where
    import Prelude
    import Wordify.Rules.Game
    import Controllers.Game.Model.ServerPlayer
    import Controllers.Game.Api
    import Control.Concurrent.STM.TChan
    import qualified Data.List.Safe as SL
    import Control.Concurrent.STM.TVar
    import Control.Monad.STM
    import Data.Text

    data ServerGame = ServerGame {
                          gameId :: Text,
                          game :: TVar Game,
                          playing :: [ServerPlayer],
                          broadcastChannel :: (TChan GameMessage),
                          numConnections :: TVar Int
                        }

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
