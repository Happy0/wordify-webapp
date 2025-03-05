module Controllers.GameLobby.GameLobby (joinClient, handleChannelMessage) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Error.Util
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Controllers.Game.Api
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Persist (deleteLobby, loadLobby, persistNewGame, persistNewLobbyPlayer)
import Controllers.GameLobby.Api
import Controllers.GameLobby.Model.GameLobby
import Data.Either
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Foundation
import Model.Api
import System.Random.Shuffle
import Prelude

{-
    Join the client to the game lobby. If the lobby is full as a result of the client joining (and it being a new user),
    the game is started and added to the database and in memory list of games, and the lobby is removed from the server.

    Returns a channel to subscribe to lobby events on, and the newly created game (if applicable)
-}
joinClient :: App -> T.Text -> ServerPlayer -> IO (Either LobbyInputError ClientLobbyJoinResult)
joinClient app gameId serverPlayer =
  do
    -- TODO - use return value rather than getting it from the cache again
    _ <- loadLobby app gameId
    result <- atomically $ updateLobbyState app gameId serverPlayer

    case result of
      Left errorMessage -> return $ Left errorMessage
      Right (ClientLobbyJoinResult broadcastChannel (Just startedGame) _) ->
        do
          _ <- startGame app gameId broadcastChannel startedGame
          return result
      Right (ClientLobbyJoinResult broadcastChannel _ previouslyJoined) -> do
        unless previouslyJoined $ persistNewLobbyPlayer (appConnPool app) gameId serverPlayer
        return result

updateLobbyState :: App -> T.Text -> ServerPlayer -> STM (Either LobbyInputError ClientLobbyJoinResult)
updateLobbyState app gameId serverPlayer =
  runExceptT $ do
    lobbyVar <- getGameLobby gameId (gameLobbies app)
    result <- lift $ handleJoinClient app gameId lobbyVar serverPlayer
    return result

startGame :: App -> T.Text -> TChan LobbyMessage -> ServerGame -> IO ()
startGame app gameId channel serverGame = do
  do
    let pool = appConnPool app
    deleteLobby app gameId
    persistNewGame pool gameId "en" serverGame
    -- Inform the clients that the game has been started
    atomically (writeTChan channel (LobbyFull gameId))

handleChannelMessage :: LobbyMessage -> LobbyResponse
handleChannelMessage (PlayerJoined serverPlayer) = Joined serverPlayer
handleChannelMessage (LobbyFull gameId) = StartGame gameId

handleJoinClient :: App -> T.Text -> TVar GameLobby -> ServerPlayer -> STM ClientLobbyJoinResult
handleJoinClient app gameId gameLobby serverPlayer =
  do
    lobby <- readTVar gameLobby
    channel <- duplicateBroadcastChannel lobby

    if inLobby lobby (identifier serverPlayer)
      then return $ ClientLobbyJoinResult channel Nothing True
      else handleJoinNewPlayer app gameId serverPlayer gameLobby

{-
    Creates a new player, adds them to the lobby, notifying the players
    of the new arrival via the broadcast channel and returns the new player.
-}
handleJoinNewPlayer :: App -> T.Text -> ServerPlayer -> TVar GameLobby -> STM ClientLobbyJoinResult
handleJoinNewPlayer app gameId newPlayer gameLobby =
  do
    lobby <- readTVar gameLobby
    let newLobby = addPlayer lobby newPlayer
    writeTVar gameLobby newLobby
    writeTChan (channel lobby) $ PlayerJoined newPlayer
    maybeServerGame <- handleLobbyFull app newLobby gameId
    channel <- duplicateBroadcastChannel newLobby

    return (ClientLobbyJoinResult channel maybeServerGame False)

handleLobbyFull :: App -> GameLobby -> T.Text -> STM (Maybe ServerGame)
handleLobbyFull app lobby gameId
  | lobbyIsFull lobby =
    do
      removeGameLobby app gameId
      serverGame <- (createGame app gameId lobby)
      return $ Just serverGame
  | otherwise =
    return Nothing

removeGameLobby :: App -> T.Text -> STM ()
removeGameLobby app gameId =
  let lobbies = gameLobbies app
   in modifyTVar lobbies $ M.delete gameId

createGame :: App -> T.Text -> GameLobby -> STM ServerGame
createGame app gameId lobby =
  do
    newChannel <- newBroadcastTChan
    newGame <- newTVar (pendingGame lobby)
    randomNumberGenerator <- readTVar (playerIdGenerator lobby)
    -- We shuffle so that who gets to go first is randomised.
    let shuffledPlayers = shuffle' players (length players) randomNumberGenerator
    numConnections <- newTVar 0
    makeServerGame gameId newGame shuffledPlayers newChannel
  where
    players = lobbyPlayers lobby

getGameLobby :: T.Text -> TVar (M.Map T.Text (TVar GameLobby)) -> ExceptT LobbyInputError STM (TVar GameLobby)
getGameLobby gameId lobbies = ExceptT (note GameLobbyDoesNotExist . M.lookup gameId <$> readTVar lobbies)
