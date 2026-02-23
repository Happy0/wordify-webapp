module Controllers.Push.PushController
  ( subscribe,
  )
where

import ClassyPrelude (IO)
import qualified Data.Text as T
import Repository.PushNotificationRepository (PushNotificationRepositoryImpl, PushSubscription (PushSubscription), saveSubscriptionImpl)
import Modules.Notifications.PushService (PushTokenSubscription (tokenEndpoint, tokenAuth, tokenP256dh, tokenExpirationTime))

subscribe :: PushNotificationRepositoryImpl -> T.Text -> T.Text -> PushTokenSubscription -> IO ()
subscribe repo userId baseHostName tokenSub =
  saveSubscriptionImpl
    repo
    (PushSubscription userId (tokenEndpoint tokenSub) (tokenAuth tokenSub) (tokenP256dh tokenSub) (tokenExpirationTime tokenSub) baseHostName)
