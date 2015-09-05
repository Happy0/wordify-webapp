module Controllers.Game.Model.ServerGame (ServerGame(ServerGame), game, playing, broadcastChannel, moveSummaries) where

    import Prelude
    import Data.Text
    import Wordify.Rules.Game
    import Model.Api
    import Controllers.Game.Model.ServerPlayer
    import Controllers.Game.Api
    import Control.Concurrent.STM.TChan
    import Controllers.Game.Model.MoveSummary

    data ServerGame = ServerGame {game :: Game, playing :: [ServerPlayer], broadcastChannel :: (TChan GameMessage), moveSummaries :: [MoveSummary] }