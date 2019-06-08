module Controllers.Game.Model.GameLobby (GameLobby(GameLobby), updatePlayers, pendingGame, lobbyPlayers, awaiting, channel, openedAt, playerIdGenerator) where

    import Prelude
    import Wordify.Rules.Game
    import Controllers.Game.Model.ServerPlayer
    import Controllers.GameLobby.Api
    import Control.Concurrent.STM.TChan
    import Controllers.GameLobby.Api
    import Data.Time.Clock
    import System.Random
    import Control.Concurrent.STM.TVar


    -- TODO: Move this module to the GameLobby folder
    data GameLobby = GameLobby {
                        pendingGame :: Game,
                        lobbyPlayers :: [ServerPlayer],
                        awaiting :: Int,
                        channel :: TChan LobbyMessage,
                        playerIdGenerator :: TVar StdGen,
                        openedAt :: UTCTime
                    }

    updatePlayers :: [ServerPlayer] -> GameLobby -> GameLobby
    updatePlayers players lobby = lobby {lobbyPlayers = players}
