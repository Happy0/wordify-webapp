module Controllers.GameLobby.GameLobby (joinClient, handleChannelMessage, sendLobbyInvite, getInvitedPlayers, getInviterForLobby) where

import ClassyPrelude (UTCTime)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import qualified Modules.Games.Api as Games
import qualified Modules.UserEvent.Api as UE
import Repository.GameRepository (GameEntity (..))
import Repository.LobbyRepository (LobbyRepository (invitePlayer, getLobbyInvites, getInviterForUser), InvitePlayerResult (..))
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Persist
import Controllers.GameLobby.Api
import Controllers.GameLobby.Model.GameLobby
import Controllers.User.Model.ServerUser (ServerUser (ServerUser))
import Controllers.User.UserController (UserController)
import qualified Controllers.User.UserController as UC
import Data.Either
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Foundation
import Modules.Notifications.Api (NotificationService, sendGameStartedNotification, createInviteNotification)
import System.Random.Shuffle
import Wordify.Rules.Game (playerNumber)
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

    case result of
      Left errorMessage -> return $ Left errorMessage
      Right (ClientLobbyJoinResult broadcastChannel (Just createdGame) _ lobby) ->
        do
          _ <- startGame app broadcastChannel createdGame
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
makeLobbyServerPlayer app gameId visitorId =
  do
    let userCtrl = userController app
    maybeUser <- UC.getUser userCtrl visitorId
    case maybeUser of
      Just serverUser -> pure $ makeNewPlayer serverUser gameId 0 Nothing
      Nothing -> pure $ makeNewPlayer (ServerUser visitorId Nothing) gameId 0 Nothing

updateLobbyState :: App -> GameLobby -> ServerPlayer -> T.Text -> UTCTime -> STM (Either LobbyInputError ClientLobbyJoinResult)
updateLobbyState app lobby serverPlayer gameId now = do
  lobbyFull <- lobbyIsFull lobby
  if lobbyFull then pure $ Left LobbyAlreadyFull else Right <$> handleJoinClient app gameId lobby serverPlayer now

startGame :: App -> TChan LobbyMessage -> GameEntity -> IO ()
startGame app channel entity = do
  let gameId = gameEntityId entity
  deleteLobby app gameId
  Games.startGame (gameService app) entity
  -- Inform the clients that the game has been started
  atomically (writeTChan channel (LobbyFull gameId))
  -- Notify user event channels about the new game
  notifyNewGame app entity
  -- Send push notifications to all players
  notifyGameStartedPush app entity

sendLobbyInvite :: LobbyRepository r => r -> GameLobby -> NotificationService -> T.Text -> T.Text -> T.Text -> T.Text -> IO InvitePlayerResult
sendLobbyInvite repo lobby notifSvc gameLobbyId invitedUsername inviterUserId inviterUsername
  | invitedUsername == inviterUsername = return InvitedSelf
  | otherwise = do
      result <- invitePlayer repo gameLobbyId invitedUsername inviterUserId
      case result of
        InvitePlayerSuccess invitedUserId -> do
          atomically $ writeTChan (channel lobby) (PlayerInvited invitedUsername inviterUsername)
          createInviteNotification notifSvc invitedUserId gameLobbyId (ServerUser inviterUserId (Just inviterUsername))
          return $ InvitePlayerSuccess invitedUserId
        InvitedUsernameNotFound -> return InvitedUsernameNotFound
        InvitedSelf -> return InvitedSelf

getInvitedPlayers :: LobbyRepository r => r -> T.Text -> IO [T.Text]
getInvitedPlayers repo = getLobbyInvites repo

getInviterForLobby :: LobbyRepository r => r -> T.Text -> T.Text -> IO (Maybe T.Text)
getInviterForLobby repo = getInviterForUser repo

handleChannelMessage :: LobbyMessage -> LobbyResponse
handleChannelMessage (PlayerJoined serverPlayer) = Joined serverPlayer
handleChannelMessage (LobbyFull gameId) = StartGame gameId
handleChannelMessage (PlayerInvited invitedPlayer invitingPlayer) = LobbyInvite invitedPlayer invitingPlayer

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

createServerGameIfLobbyFull :: GameLobby -> T.Text -> UTCTime -> STM (Maybe GameEntity)
createServerGameIfLobbyFull lobby gameId now = do
  lobbyFull <- lobbyIsFull lobby
  if lobbyFull
    then Just <$> createGame gameId lobby now
    else return Nothing

createGame :: T.Text -> GameLobby -> UTCTime -> STM GameEntity
createGame gameId lobby now = do
  players <- readTVar (lobbyPlayers lobby)
  randomNumberGenerator <- readTVar (playerIdGenerator lobby)
  -- We shuffle so that who gets to go first is randomised.
  let shuffledPlayers = shuffle' players (length players) randomNumberGenerator
  return $ GameEntity
    { gameEntityId = gameId
    , gameEntityGame = pendingGame lobby
    , gameEntityPlayers = map user shuffledPlayers
    , gameEntityCreatedAt = now
    , gameEntityLastMoveMadeAt = Nothing
    , gameEntityFinishedAt = Nothing
    , gameEntitySetup = pendingGameSetup lobby
    }

notifyGameStartedPush :: App -> GameEntity -> IO ()
notifyGameStartedPush app entity = do
  let pushCtrl = notificationService app
      gId = gameEntityId entity
  forM_ (gameEntityPlayers entity) $ \(ServerUser uid _) ->
    sendGameStartedNotification pushCtrl uid gId

notifyNewGame :: App -> GameEntity -> IO ()
notifyNewGame app entity = do
  let svc = userEventService app
      gId = gameEntityId entity
      players = gameEntityPlayers entity
  serverGame <- atomically $ makeServerGame
    gId
    (gameEntityGame entity)
    (map (\su -> makeNewPlayer su gId 0 Nothing) players)
    (gameEntityCreatedAt entity)
    (gameEntityLastMoveMadeAt entity)
    (gameEntityFinishedAt entity)
    (gameEntitySetup entity)
  forM_ players $ \(ServerUser uid _) ->
    atomically $ UE.notifyNewGame svc uid gId serverGame