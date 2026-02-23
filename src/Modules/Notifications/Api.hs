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

import ClassyPrelude (IO, Maybe, pure, (.))
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.HTTP.Client (Manager)
import Repository.NotificationRepository (Notification, NotificationRepository)
import Repository.PushNotificationRepository (PushNotificationRepositoryImpl)
import Web.WebPush (VAPIDKeys)
import Modules.Notifications.PushService (PushController, PushTokenSubscription (..))
import qualified Modules.Notifications.PushService as PushService
import qualified Modules.Notifications.NotificationService as NS

data NotificationService = NotificationService
  { pushService :: PushController,
    notifService :: NS.NotificationService
  }

makeNotificationService :: NotificationRepository a => a -> PushNotificationRepositoryImpl -> Maybe VAPIDKeys -> Manager -> IO NotificationService
makeNotificationService notifRepo pushRepo keys manager = do
  pushCtrl <- PushService.makePushController pushRepo keys manager
  pure (NotificationService pushCtrl (NS.makeNotificationService notifRepo))

sendMoveNotification :: NotificationService -> T.Text -> T.Text -> IO ()
sendMoveNotification svc = PushService.sendMoveNotification (pushService svc)

sendGameStartedNotification :: NotificationService -> T.Text -> T.Text -> IO ()
sendGameStartedNotification svc = PushService.sendGameStartedNotification (pushService svc)

sendGameOverNotification :: NotificationService -> T.Text -> T.Text -> IO ()
sendGameOverNotification svc = PushService.sendGameOverNotification (pushService svc)

createInviteNotification :: NotificationService -> T.Text -> T.Text -> T.Text -> IO ()
createInviteNotification svc userId lobbyId invitedByUserId = do
  NS.createInviteNotification (notifService svc) userId lobbyId invitedByUserId
  PushService.createInviteNotification (pushService svc) userId lobbyId

getRecentNotifications :: NotificationService -> T.Text -> IO [Notification]
getRecentNotifications svc = NS.getRecentNotifications (notifService svc)

markNotificationsAsRead :: NotificationService -> T.Text -> UTCTime -> IO ()
markNotificationsAsRead svc = NS.markNotificationsAsRead (notifService svc)

pushNotificationRepository :: NotificationService -> PushNotificationRepositoryImpl
pushNotificationRepository = PushService.pushNotificationRepository . pushService
