module Controllers.GameLobby.GameLobby (joinClient, handleChannelMessage) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Error.Util
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Controllers.Common.CacheableSharedResource
import Controllers.Game.Api
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Persist (deleteLobby, getLobbyPlayer, persistNewGame, persistNewLobbyPlayer)
import Controllers.GameLobby.Api
import Controllers.GameLobby.Model.GameLobby
import Controllers.User.Model.AuthUser
import Controllers.User.Persist
import Data.Either
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Foundation
import Model.Api
import System.Random.Shuffle
import UnliftIO.Resource
import qualified Wordify.Rules.Game as G
import qualified Wordify.Rules.Player as P
import Prelude

{-
    Join the client to the game lobby. If the lobby is full as a result of the client joining (and it being a new user),
    the game is started and added to the database and in memory list of games, and the lobby is removed from the server.

    Returns a channel to subscribe to lobby events on, and the newly created game (if applicable)
-}
joinClient :: App -> GameLobby -> T.Text -> T.Text -> IO (Either LobbyInputError ClientLobbyJoinResult)
joinClient app gameLobby gameId userId =
  do
    serverPlayer <- getLobbyServerPlayer app gameLobby gameId userId
    result <- atomically $ updateLobbyState app gameLobby serverPlayer gameId

    case result of
      Left errorMessage -> return $ Left errorMessage
      Right (ClientLobbyJoinResult broadcastChannel (Just startedGame) _) ->
        do
          _ <- startGame app gameId broadcastChannel startedGame
          return result
      Right (ClientLobbyJoinResult broadcastChannel _ previouslyJoined) ->
        unless
          previouslyJoined
          ( persistNewLobbyPlayer
              (appConnPool app)
              gameId
              serverPlayer
          )
          >> pure result

getLobbyServerPlayer :: App -> GameLobby -> T.Text -> T.Text -> IO ServerPlayer
getLobbyServerPlayer app gameLobby gameId userId = do
  existingPlayer <- existingServerPlayer app gameId userId
  case existingPlayer of
    Just player -> pure $ player
    Nothing ->
      do
        let pool = appConnPool app
        user <- getUser pool userId
        case user of
          Just (AuthUser ident nickname) -> pure $ makeNewPlayer nickname gameId ident False Nothing
          Nothing -> pure $ makeNewPlayer Nothing userId gameId False Nothing

existingServerPlayer :: App -> T.Text -> T.Text -> IO (Maybe ServerPlayer)
existingServerPlayer app gameId userId = getLobbyPlayer (appConnPool app) gameId userId

updateLobbyState :: App -> GameLobby -> ServerPlayer -> T.Text -> STM (Either LobbyInputError ClientLobbyJoinResult)
updateLobbyState app lobby serverPlayer gameId = do
  lobbyFull <- lobbyIsFull lobby

  if lobbyFull then pure $ Left LobbyAlreadyFull else Right <$> handleJoinClient app gameId lobby serverPlayer

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

handleJoinClient :: App -> T.Text -> GameLobby -> ServerPlayer -> STM ClientLobbyJoinResult
handleJoinClient app gameId gameLobby serverPlayer =
  do
    channel <- duplicateBroadcastChannel gameLobby

    playerInLobby <- inLobby gameLobby (playerId serverPlayer)
    if playerInLobby
      then return $ ClientLobbyJoinResult channel Nothing True
      else handleJoinNewPlayer app gameId serverPlayer gameLobby

{-
    Creates a new player, adds them to the lobby, notifying the players
    of the new arrival via the broadcast channel and returns the new player.
-}
handleJoinNewPlayer :: App -> T.Text -> ServerPlayer -> GameLobby -> STM ClientLobbyJoinResult
handleJoinNewPlayer app gameId newPlayer gameLobby =
  do
    newLobby <- addPlayer gameLobby newPlayer
    writeTChan (channel gameLobby) $ PlayerJoined newPlayer
    maybeServerGame <- handleLobbyFull app newLobby gameId
    channel <- duplicateBroadcastChannel newLobby

    return (ClientLobbyJoinResult channel maybeServerGame False)

handleLobbyFull :: App -> GameLobby -> T.Text -> STM (Maybe ServerGame)
handleLobbyFull app lobby gameId = do
  lobbyFull <- lobbyIsFull lobby
  if lobbyFull
    then Just <$> (createGame app gameId lobby)
    else return Nothing

createGame :: App -> T.Text -> GameLobby -> STM ServerGame
createGame app gameId lobby =
  do
    players <- readTVar (lobbyPlayers lobby)
    let initialGameState = pendingGame lobby

    newChannel <- newBroadcastTChan
    newGame <- newTVar initialGameState
    randomNumberGenerator <- readTVar (playerIdGenerator lobby)

    -- We shuffle so that who gets to go first is randomised.
    let shuffledPlayers = shuffle' players (length players) randomNumberGenerator
    makeServerGame gameId newGame shuffledPlayers newChannel