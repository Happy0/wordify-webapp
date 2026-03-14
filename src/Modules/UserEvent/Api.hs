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
import Controllers.Common.CacheableSharedResource (ResourceCache, getCacheableResource, makeGlobalResourceCache, withPeekCacheableResource)
import Data.Time (UTCTime)
import Controllers.Game.Model.ServerGame (ServerGame, ServerGameSnapshot)
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

notifyMove :: UserEventService -> Text -> Text -> ServerGameSnapshot -> UTCTime -> STM ()
notifyMove service userId gameId serverGame now =
  withPeekCacheableResource (userEventChannels service) userId action now
  where
    action Nothing = return ()
    action (Just chan) = writeTChan chan (MoveInUserGame gameId serverGame)

notifyGameOver :: UserEventService -> Text -> Text -> ServerGameSnapshot -> UTCTime -> STM ()
notifyGameOver service userId gameId serverGame now =
  withPeekCacheableResource (userEventChannels service) userId action now
  where
    action Nothing = return ()
    action (Just chan) = writeTChan chan (GameOver gameId serverGame)

notifyNewGame :: UserEventService -> Text -> Text -> ServerGameSnapshot -> UTCTime -> STM ()
notifyNewGame service userId gameId serverGame now =
  withPeekCacheableResource (userEventChannels service) userId action now
  where
    action Nothing = return ()
    action (Just chan) = writeTChan chan (NewGame gameId serverGame)

notifyPlayerActivityChanged :: UserEventService -> Text -> Text -> [Text] -> UTCTime -> STM ()
notifyPlayerActivityChanged service userId gameId activeNames now =
  withPeekCacheableResource (userEventChannels service) userId action now
  where
    action Nothing = return ()
    action (Just chan) = writeTChan chan (PlayerActivityChanged gameId activeNames)

notifyNotificationAdded :: UserEventService -> Text -> Notification -> UTCTime -> STM ()
notifyNotificationAdded service userId notification now =
  withPeekCacheableResource (userEventChannels service) userId action now
  where
    action Nothing = return ()
    action (Just chan) = writeTChan chan (NotificationsChanged (NotificationAdded notification))

notifyNotificationsRead :: UserEventService -> Text -> [Text] -> UTCTime -> STM ()
notifyNotificationsRead service userId notificationIds now =
  withPeekCacheableResource (userEventChannels service) userId action now
  where
    action Nothing = return ()
    action (Just chan) = writeTChan chan (NotificationsChanged (NotificationsRead notificationIds))

subscribeToUserChannel :: MonadResource m => UserEventService -> Text -> m (ReleaseKey, Either Text (TChan UserEvent))
subscribeToUserChannel service userId = do
  (releaseKey, result) <- getCacheableResource (userEventChannels service) userId
  case result of
    Left err -> return (releaseKey, Left err)
    Right channel -> do
      subscription <- liftIO $ atomically (dupTChan channel)
      return (releaseKey, Right subscription)
