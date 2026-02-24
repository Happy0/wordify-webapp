module Modules.UserEvent.Api
  ( UserEventService
  , makeUserEventService
  , notifyMove
  , notifyGameOver
  , notifyNewGame
  , notifyPlayerActivityChanged
  , notifyNotificationAdded
  , notifyNotificationsRead
  , subscribeToUserChannel
  ) where

import ClassyPrelude (IO, Maybe (..), Either (..), pure, return, (<$>), ($), liftIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan, dupTChan)
import Controllers.Common.CacheableSharedResource (ResourceCache, getCacheableResource, makeGlobalResourceCache, peekCacheableResource)
import Controllers.Game.Model.ServerGame (ServerGame)
import Controllers.Game.Model.UserEventSubscription (UserEvent (..), NotificationUpdate (..), newUserEventSubcriptionChannel)
import Repository.NotificationRepository (Notification)
import Data.Text (Text)
import UnliftIO.Resource (MonadResource, ReleaseKey)

newtype UserEventService = UserEventService
  { userEventChannels :: ResourceCache Text (TChan UserEvent) }

makeUserEventService :: IO UserEventService
makeUserEventService = do
  let loadChannel :: Text -> IO (Either Text (TChan UserEvent))
      loadChannel _ = Right <$> newUserEventSubcriptionChannel
  cache <- makeGlobalResourceCache loadChannel Nothing
  pure (UserEventService cache)

notifyMove :: UserEventService -> Text -> Text -> ServerGame -> STM ()
notifyMove service userId gameId serverGame = do
  maybeChannel <- peekCacheableResource (userEventChannels service) userId
  case maybeChannel of
    Nothing -> return ()
    Just chan -> writeTChan chan (MoveInUserGame gameId serverGame)

notifyGameOver :: UserEventService -> Text -> Text -> ServerGame -> STM ()
notifyGameOver service userId gameId serverGame = do
  maybeChannel <- peekCacheableResource (userEventChannels service) userId
  case maybeChannel of
    Nothing -> return ()
    Just chan -> writeTChan chan (GameOver gameId serverGame)

notifyNewGame :: UserEventService -> Text -> Text -> ServerGame -> STM ()
notifyNewGame service userId gameId serverGame = do
  maybeChannel <- peekCacheableResource (userEventChannels service) userId
  case maybeChannel of
    Nothing -> return ()
    Just chan -> writeTChan chan (NewGame gameId serverGame)

notifyPlayerActivityChanged :: UserEventService -> Text -> Text -> [Text] -> STM ()
notifyPlayerActivityChanged service userId gameId activeNames = do
  maybeChannel <- peekCacheableResource (userEventChannels service) userId
  case maybeChannel of
    Nothing -> return ()
    Just chan -> writeTChan chan (PlayerActivityChanged gameId activeNames)

notifyNotificationAdded :: UserEventService -> Text -> Notification -> STM ()
notifyNotificationAdded service userId notification = do
  maybeChannel <- peekCacheableResource (userEventChannels service) userId
  case maybeChannel of
    Nothing -> return ()
    Just chan -> writeTChan chan (NotificationsChanged (NotificationAdded notification))

notifyNotificationsRead :: UserEventService -> Text -> [Text] -> STM ()
notifyNotificationsRead service userId notificationIds = do
  maybeChannel <- peekCacheableResource (userEventChannels service) userId
  case maybeChannel of
    Nothing -> return ()
    Just chan -> writeTChan chan (NotificationsChanged (NotificationsRead notificationIds))

subscribeToUserChannel :: MonadResource m => UserEventService -> Text -> m (ReleaseKey, Either Text (TChan UserEvent))
subscribeToUserChannel service userId = do
  (releaseKey, result) <- getCacheableResource (userEventChannels service) userId
  case result of
    Left err -> return (releaseKey, Left err)
    Right channel -> do
      subscription <- liftIO $ atomically (dupTChan channel)
      return (releaseKey, Right subscription)
