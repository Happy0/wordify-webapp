module Controllers.Game.Model.ServerGame (ServerGame(ServerGame), game, playing, broadcastChannel, moveSummaries, getServerPlayer) where

    import Prelude
    import Data.Text
    import Wordify.Rules.Game
    import Model.Api
    import Controllers.Game.Model.ServerPlayer
    import Controllers.Game.Api
    import Control.Concurrent.STM.TChan
    import Controllers.Game.Model.MoveSummary
    import qualified Data.List.Safe as SL

    data ServerGame = ServerGame {game :: Game, playing :: [ServerPlayer], broadcastChannel :: (TChan GameMessage), moveSummaries :: [MoveSummary] }

    getServerPlayer :: ServerGame -> Int -> Maybe ServerPlayer
    getServerPlayer serverGame playerNumber = playing serverGame SL.!! (playerNumber - 1)
