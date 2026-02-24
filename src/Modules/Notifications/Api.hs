module Modules.Notifications.Api
  ( NotificationService,
    makeNotificationService,
    sendMoveNotification,
    sendGameStartedNotification,
    sendGameOverNotification,
    createInviteNotification,
    getRecentNotifications,
    markNotificationsAsRead,
    PushTokenSubscription (PushTokenSubscription, tokenAuth, tokenP256dh, tokenEndpoint, tokenExpirationTime),
    pushNotificationRepository,
  )
where

import ClassyPrelude (IO, Maybe, pure, (.), ($), map, tshow)
import Control.Concurrent.STM (atomically)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.HTTP.Client (Manager)
import Repository.NotificationRepository (Notification, NotificationId, NotificationRepository)
import Controllers.User.Model.ServerUser (ServerUser)
import Modules.UserEvent.Api (UserEventService, notifyNotificationAdded, notifyNotificationsRead)
import Repository.PushNotificationRepository (PushNotificationRepositoryImpl)
import Web.WebPush (VAPIDKeys)
import Modules.Notifications.PushService (PushController, PushTokenSubscription (..))
import qualified Modules.Notifications.PushService as PushService
import qualified Modules.Notifications.NotificationService as NS

data NotificationService = NotificationService
  { pushService :: PushController,
    notifService :: NS.NotificationService,
    userEventService :: UserEventService
  }

makeNotificationService :: NotificationRepository a => a -> PushNotificationRepositoryImpl -> Maybe VAPIDKeys -> Manager -> UserEventService -> IO NotificationService
makeNotificationService notifRepo pushRepo keys manager userEventSvc = do
  pushCtrl <- PushService.makePushController pushRepo keys manager
  pure (NotificationService pushCtrl (NS.makeNotificationService notifRepo) userEventSvc)

sendMoveNotification :: NotificationService -> T.Text -> T.Text -> IO ()
sendMoveNotification svc = PushService.sendMoveNotification (pushService svc)

sendGameStartedNotification :: NotificationService -> T.Text -> T.Text -> IO ()
sendGameStartedNotification svc = PushService.sendGameStartedNotification (pushService svc)

sendGameOverNotification :: NotificationService -> T.Text -> T.Text -> IO ()
sendGameOverNotification svc = PushService.sendGameOverNotification (pushService svc)

createInviteNotification :: NotificationService -> T.Text -> T.Text -> ServerUser -> IO ()
createInviteNotification svc userId lobbyId invitedByUser = do
  notification <- NS.createInviteNotification (notifService svc) userId lobbyId invitedByUser
  atomically $ notifyNotificationAdded (userEventService svc) userId notification
  PushService.createInviteNotification (pushService svc) userId lobbyId

getRecentNotifications :: NotificationService -> T.Text -> IO [Notification]
getRecentNotifications svc = NS.getRecentNotifications (notifService svc)

markNotificationsAsRead :: NotificationService -> T.Text -> UTCTime -> IO [NotificationId]
markNotificationsAsRead svc userId upTo = do
  ids <- NS.markNotificationsAsRead (notifService svc) userId upTo
  atomically $ notifyNotificationsRead (userEventService svc) userId (map tshow ids)
  pure ids

pushNotificationRepository :: NotificationService -> PushNotificationRepositoryImpl
pushNotificationRepository = PushService.pushNotificationRepository . pushService
