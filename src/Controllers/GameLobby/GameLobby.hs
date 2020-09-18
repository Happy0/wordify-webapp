module Controllers.GameLobby.GameLobby (setupPrequisets, startGame, handleChannelMessage) where

    import Prelude
    import Foundation
    import Control.Concurrent.STM.TVar
    import Controllers.GameLobby.Api
    import Controllers.Game.Model.ServerPlayer
    import Controllers.GameLobby.Api
    import Model.Api
    import Controllers.GameLobby.Model.GameLobby
    import qualified Data.Text as T
    import Control.Concurrent.STM.TChan
    import Control.Monad
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import qualified Data.Map as M
    import Controllers.Game.Model.ServerGame
    import Controllers.Game.Api
    import Control.Monad.Trans.Except
    import Control.Error.Util
    import qualified Data.List as L
    import Control.Monad.Trans.Class
    import Data.Either
    import Controllers.Game.Persist
    import System.Random.Shuffle

    {-
        Sets up the broadcast channel for servicing the websocket, returning:
        * An ID for the new player if they have not joined the lobby before and received a cookie.
        * A message channel for listening for people joining the lobby.
        * If the lobby becomes full due to the player joining, the new server game
    -}
    setupPrequisets :: App -> T.Text -> Maybe T.Text -> STM (Either LobbyInputError (T.Text, TChan LobbyMessage, Maybe ServerGame))
    setupPrequisets app gameId maybePlayerId =
        runExceptT $ do
                lobbyVar <- getGameLobby gameId (gameLobbies app)
                currentLobby <- lift $ readTVar lobbyVar
                broadcastChan <- lift . dupTChan . channel $ currentLobby
                case maybePlayerId of
                    Nothing -> do
                            (newPlayer, maybeNewGame) <- lift $ handleJoinNewPlayer app gameId lobbyVar
                            return (identifier newPlayer, broadcastChan, maybeNewGame)
                    Just playerId -> do
                        _ <- hoistEither (getExistingPlayer currentLobby playerId)
                        return (playerId, broadcastChan, Nothing)

    startGame :: App -> T.Text -> TChan LobbyMessage -> ServerGame -> IO ()
    startGame app gameId channel serverGame = do
      do
          let pool = appConnPool app
          persistNewGame pool gameId "en" serverGame
          -- Inform the clients that the game has been started
          atomically (writeTChan channel (LobbyFull gameId))

    handleChannelMessage :: LobbyMessage -> LobbyResponse
    handleChannelMessage (PlayerJoined serverPlayer) = Joined serverPlayer
    handleChannelMessage (LobbyFull gameId) = StartGame gameId

    {-
        Creates a new player, adds them to the lobby, notifying the players
        of the new arrival via the broadcast channel and returns the new player.
    -}
    handleJoinNewPlayer :: App -> T.Text -> TVar GameLobby -> STM (ServerPlayer, Maybe ServerGame)
    handleJoinNewPlayer app gameId gameLobby =
        do
            lobby <- readTVar gameLobby
            newPlayerIdGenerator <- readTVar $ playerIdGenerator lobby
            let (playerId, newGen) = makeNewPlayerId newPlayerIdGenerator
            writeTVar (playerIdGenerator lobby) newGen

            let players = lobbyPlayers lobby
            let newPlayerName =  T.concat ["player", T.pack . show $ (length players + 1)]
            let newPlayer = makeNewPlayer newPlayerName playerId
            let newPlayers = players ++ [newPlayer]
            let newLobby = updatePlayers newPlayers lobby

            writeTVar gameLobby newLobby

            writeTChan (channel lobby) $ PlayerJoined newPlayer
            maybeServerGame <- handleLobbyFull app newLobby gameId

            return (newPlayer, maybeServerGame)

    handleLobbyFull :: App -> GameLobby -> T.Text -> STM (Maybe ServerGame)
    handleLobbyFull app lobby gameId =
        if (length (lobbyPlayers lobby) == (awaiting lobby)) then
            do
                -- Broadcast that the game is ready to begin
                let broadcastChannel = channel lobby
                removeGameLobby app gameId
                serverGame <- (createGame app gameId lobby)
                return $ Just serverGame
        else
            return Nothing

    removeGameLobby :: App -> T.Text -> STM ()
    removeGameLobby app gameId =
        let lobbies = gameLobbies app
        in modifyTVar lobbies $ M.delete gameId

    createGame :: App -> T.Text -> GameLobby -> STM ServerGame
    createGame app gameId lobby =
        do
            let gamesInProgress = games app
            newChannel <- newBroadcastTChan
            newGame <- newTVar (pendingGame lobby)
            randomNumberGenerator <- readTVar (playerIdGenerator lobby)
            -- We shuffle so that who gets to go first is randomised.
            let shuffledPlayers = shuffle' players (length players) randomNumberGenerator
            numConnections <- newTVar 0
            serverGame <- makeServerGame gameId newGame shuffledPlayers newChannel
            return serverGame
        where
            players = lobbyPlayers lobby
       
    getGameLobby :: T.Text -> TVar (M.Map T.Text (TVar GameLobby)) -> ExceptT LobbyInputError STM (TVar GameLobby)
    getGameLobby gameId lobbies = ExceptT (note GameLobbyDoesNotExist . M.lookup gameId <$> readTVar lobbies)

    getExistingPlayer :: GameLobby -> T.Text -> Either LobbyInputError ServerPlayer
    getExistingPlayer lobby playerId = note InvalidPlayerID $ L.find (\player -> identifier player == playerId) players
        where
            players = lobbyPlayers lobby
