module Controllers.GameLobby.Model.GameLobby (
    GameLobby(GameLobby),
    ClientLobbyJoinResult(ClientLobbyJoinResult, broadcastChannel),
     gameStarted,
     addPlayer,
     pendingGame,
     lobbyPlayers,
     awaiting,
     channel,
     openedAt,
     playerIdGenerator,
     duplicateBroadcastChannel,
     inLobby,
     lobbyIsFull) where

    import Prelude
    import Wordify.Rules.Game
    import Controllers.Game.Model.ServerPlayer
    import Controllers.Game.Model.ServerGame
    import Controllers.GameLobby.Api
    import Control.Concurrent.STM.TChan
    import Controllers.GameLobby.Api
    import Data.Time.Clock
    import System.Random
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import qualified Data.Text as T
    import Data.List
    import Data.Maybe
    
    data ClientLobbyJoinResult = ClientLobbyJoinResult {
        -- For listening and sending events to the game lobby from the client
        broadcastChannel :: TChan LobbyMessage,

        -- If the lobby was made full as a result of the client join, the newly created server game
        createdGame :: Maybe ServerGame
    }

    data GameLobby = GameLobby {
                        pendingGame :: Game,
                        lobbyPlayers :: [ServerPlayer],
                        awaiting :: Int,
                        channel :: TChan LobbyMessage,
                        -- TODO: rename this to 'lobbyGenerator'
                        playerIdGenerator :: TVar StdGen,
                        openedAt :: UTCTime
                    }

    gameStarted :: ClientLobbyJoinResult -> Bool
    gameStarted lobbyJoinResult = isJust $ (createdGame lobbyJoinResult)

    duplicateBroadcastChannel :: GameLobby -> STM (TChan LobbyMessage)
    duplicateBroadcastChannel gameLobby = dupTChan . channel $ gameLobby

    addPlayer :: GameLobby -> ServerPlayer -> GameLobby
    addPlayer gameLobby newPlayer = 
        let newPlayers = (lobbyPlayers gameLobby) ++ [newPlayer]
        in updatePlayers gameLobby newPlayers

    lobbyIsFull :: GameLobby -> Bool
    lobbyIsFull lobby = length (lobbyPlayers lobby) == (awaiting lobby)

    updatePlayers :: GameLobby -> [ServerPlayer] -> GameLobby
    updatePlayers lobby players = lobby {lobbyPlayers = players}

    inLobby :: GameLobby -> T.Text -> Bool
    inLobby lobby playerIdentifier = isJust (find isPlayer (lobbyPlayers lobby))
        where
            isPlayer player = (identifier player) == playerIdentifier
