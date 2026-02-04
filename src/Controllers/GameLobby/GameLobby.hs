module Controllers.GameLobby.GameLobby (joinClient, handleChannelMessage) where

import ClassyPrelude (UTCTime)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Persist
import Controllers.GameLobby.Api
import Controllers.GameLobby.Model.GameLobby
import Controllers.User.Model.AuthUser
import Controllers.User.Persist
import Data.Either
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Foundation
import System.Random.Shuffle
import Prelude

{-
    Join the client to the game lobby. If the lobby is full as a result of the client joining (and it being a new user),
    the game is started and added to the database and in memory list of games, and the lobby is removed from the server.

    Returns a channel to subscribe to lobby events on, and the newly created game (if applicable)
-}
joinClient :: App -> GameLobby -> T.Text -> T.Text -> IO (Either LobbyInputError ClientLobbyJoinResult)
joinClient app gameLobby gameId userId =
  do
    now <- getCurrentTime
    serverPlayer <- makeLobbyServerPlayer app gameId userId
    result <- atomically $ updateLobbyState app gameLobby serverPlayer gameId now
    let language = gameLanguage gameLobby

    case result of
      Left errorMessage -> return $ Left errorMessage
      Right (ClientLobbyJoinResult broadcastChannel (Just createdGame) _ lobby) ->
        do
          _ <- startGame app gameId language broadcastChannel createdGame
          return result
      Right (ClientLobbyJoinResult broadcastChannel _ previouslyJoined lobby) ->
        unless
          previouslyJoined
          ( persistNewLobbyPlayer
              (appConnPool app)
              gameId
              serverPlayer
          )
          >> pure result

makeLobbyServerPlayer :: App -> T.Text -> T.Text -> IO ServerPlayer
makeLobbyServerPlayer app gameId userId =
  do
    let pool = appConnPool app
    user <- getUser pool userId
    case user of
      -- TODO: proper connection tracking
      Just (AuthUser ident nickname) -> pure $ makeNewPlayer nickname gameId ident 0 Nothing
      Nothing -> pure $ makeNewPlayer Nothing gameId userId 0 Nothing

updateLobbyState :: App -> GameLobby -> ServerPlayer -> T.Text -> UTCTime -> STM (Either LobbyInputError ClientLobbyJoinResult)
updateLobbyState app lobby serverPlayer gameId now = do
  lobbyFull <- lobbyIsFull lobby
  if lobbyFull then pure $ Left LobbyAlreadyFull else Right <$> handleJoinClient app gameId lobby serverPlayer now

startGame :: App -> T.Text -> T.Text -> TChan LobbyMessage -> ServerGame -> IO ()
startGame app gameId gameLanguage channel serverGame = do
  do
    let pool = appConnPool app
    deleteLobby app gameId
    persistNewGame pool gameId gameLanguage serverGame
    -- Inform the clients that the game has been started
    atomically (writeTChan channel (LobbyFull gameId))

handleChannelMessage :: LobbyMessage -> LobbyResponse
handleChannelMessage (PlayerJoined serverPlayer) = Joined serverPlayer
handleChannelMessage (LobbyFull gameId) = StartGame gameId

handleJoinClient :: App -> T.Text -> GameLobby -> ServerPlayer -> UTCTime -> STM ClientLobbyJoinResult
handleJoinClient app gameId gameLobby serverPlayer now =
  do
    channel <- duplicateBroadcastChannel gameLobby

    playerInLobby <- inLobby gameLobby (playerId serverPlayer)
    if playerInLobby
      then do
        lobbySnapshot <- takeLobbySnapshot gameLobby
        pure (ClientLobbyJoinResult channel Nothing True lobbySnapshot)
      else handleJoinNewPlayer app gameId serverPlayer gameLobby now

{-
    Creates a new player, adds them to the lobby, notifying the players
    of the new arrival via the broadcast channel and returns the new player.
-}
handleJoinNewPlayer :: App -> T.Text -> ServerPlayer -> GameLobby -> UTCTime -> STM ClientLobbyJoinResult
handleJoinNewPlayer app gameId newPlayer gameLobby now =
  do
    playerAdded <- addPlayer gameLobby newPlayer
    writeTChan (channel gameLobby) $ PlayerJoined newPlayer
    maybeServerGame <- createServerGameIfLobbyFull gameLobby gameId now
    channel <- duplicateBroadcastChannel gameLobby
    lobbySnapshot <- takeLobbySnapshot gameLobby

    return (ClientLobbyJoinResult channel maybeServerGame (not playerAdded) lobbySnapshot)

createServerGameIfLobbyFull :: GameLobby -> T.Text -> UTCTime -> STM (Maybe ServerGame)
createServerGameIfLobbyFull lobby gameId now = do
  lobbyFull <- lobbyIsFull lobby
  if lobbyFull
    then Just <$> createGame gameId lobby now
    else return Nothing

createGame :: T.Text -> GameLobby -> UTCTime -> STM ServerGame
createGame gameId lobby now =
  do
    players <- readTVar (lobbyPlayers lobby)
    let initialGameState = pendingGame lobby

    randomNumberGenerator <- readTVar (playerIdGenerator lobby)

    -- We shuffle so that who gets to go first is randomised.
    let shuffledPlayers = shuffle' players (length players) randomNumberGenerator
    makeNewServerGame gameId initialGameState shuffledPlayers now (pendingGameSetup lobby)