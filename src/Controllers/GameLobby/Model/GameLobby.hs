module Controllers.GameLobby.Model.GameLobby (
    GameLobby(GameLobby),
     addPlayer,
     pendingGame,
     lobbyPlayers,
     awaiting,
     channel,
     openedAt,
     playerIdGenerator,
     inLobby) where

    import Prelude
    import Wordify.Rules.Game
    import Controllers.Game.Model.ServerPlayer
    import Controllers.GameLobby.Api
    import Control.Concurrent.STM.TChan
    import Controllers.GameLobby.Api
    import Data.Time.Clock
    import System.Random
    import Control.Concurrent.STM.TVar
    import qualified Data.Text as T
    import Data.List
    import Data.Maybe

    -- TODO: Move this module to the GameLobby folder
    data GameLobby = GameLobby {
                        pendingGame :: Game,
                        lobbyPlayers :: [ServerPlayer],
                        awaiting :: Int,
                        channel :: TChan LobbyMessage,
                        -- TODO: rename this to 'lobbyGenerator'
                        playerIdGenerator :: TVar StdGen,
                        openedAt :: UTCTime
                    }

    addPlayer :: GameLobby -> ServerPlayer -> GameLobby
    addPlayer gameLobby newPlayer = 
        let newPlayers = (lobbyPlayers gameLobby) ++ [newPlayer]
        in updatePlayers gameLobby newPlayers

    updatePlayers :: GameLobby -> [ServerPlayer] -> GameLobby
    updatePlayers lobby players = lobby {lobbyPlayers = players}

    inLobby :: GameLobby -> T.Text -> Bool
    inLobby lobby playerIdentifier = isJust (find isPlayer (lobbyPlayers lobby))
        where
            isPlayer player = (identifier player) == playerIdentifier
