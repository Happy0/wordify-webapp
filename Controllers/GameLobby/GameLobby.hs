module Controllers.GameLobby.GameLobby (handleChannelMessage, handleClientMessage, handleJoinNewPlayer, handleLobbyFull) where

    import Prelude
    import Foundation
    import Control.Concurrent.STM.TVar
    import Controllers.GameLobby.Api
    import Controllers.Game.Model.ServerPlayer
    import Controllers.GameLobby.Api
    import Model.Api
    import Controllers.Game.Model.GameLobby
    import qualified Data.Text as T
    import Control.Concurrent.STM.TChan
    import Control.Monad
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import qualified Data.Map as M

    handleChannelMessage :: LobbyMessage -> LobbyResponse
    handleChannelMessage (PlayerJoined serverPlayer) = Joined serverPlayer

    handleClientMessage :: App -> T.Text -> ClientRequest -> IO (Either T.Text LobbyResponse)
    handleClientMessage app gameId (Join Nothing) = undefined
    handleClientMessage app gameId (Join (Just textId)) = undefined

    {-
        Creates a new player, adds them to the lobby, and returns the new player.
    -}
    handleJoinNewPlayer :: TVar GameLobby -> STM ServerPlayer
    handleJoinNewPlayer gameLobby =
        do
            lobby <- readTVar gameLobby
            newPlayerIdGenerator <- readTVar $ playerIdGenerator lobby
            let (playerId, newGen) = makeNewPlayerId newPlayerIdGenerator
            writeTVar (playerIdGenerator lobby) newGen

            let players = lobbyPlayers lobby
            let newPlayerName =  T.concat ["player", T.pack . show $ length players]
            let newPlayer = makeNewPlayer newPlayerName playerId
            let newPlayers = players ++ [newPlayer]
            
            modifyTVar gameLobby $ updatePlayers newPlayers
            return newPlayer

    handleLobbyFull :: App -> GameLobby -> T.Text -> STM ()
    handleLobbyFull app lobby gameId =
        when (length (lobbyPlayers lobby) == (awaiting lobby)) $
            do
                -- Broadcast that the game is ready to begin
                let broadcastChannel = channel lobby
                writeTChan broadcastChannel LobbyFull

                removeGameLobby app gameId
                createGame app gameId lobby


    removeGameLobby :: App -> T.Text -> STM ()
    removeGameLobby app gameId =
        let lobbies = gameLobbies app
        in modifyTVar lobbies $ M.delete gameId


    createGame :: App -> T.Text -> GameLobby -> STM ()
    createGame app gameId lobby = undefined

