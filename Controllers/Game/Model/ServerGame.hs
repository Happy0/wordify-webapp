module Controllers.Game.Model.ServerGame (ServerGame(ServerGame), game, playing, broadcastChannel, getServerPlayer) where

    import Prelude
    import Data.Text
    import Wordify.Rules.Game
    import Model.Api
    import Controllers.Game.Model.ServerPlayer
    import Controllers.Game.Api
    import Control.Concurrent.STM.TChan
    import qualified Data.List.Safe as SL
    import Control.Concurrent.STM.TVar

    data ServerGame = ServerGame {game :: TVar Game, playing :: [ServerPlayer], broadcastChannel :: (TChan GameMessage)}

    getServerPlayer :: ServerGame -> Int -> Maybe ServerPlayer
    getServerPlayer serverGame playerNumber = playing serverGame SL.!! (playerNumber - 1)
