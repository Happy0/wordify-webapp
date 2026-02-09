module Controllers.Push.PushController
  ( PushController,
    makePushController,
    subscribe,
    PushTokenSubscription (PushTokenSubscription, tokenAuth, tokenP256dh, tokenEndpoint, tokenExpirationTime),
  )
where

import ClassyPrelude (IO, Maybe)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Repository.PushNotificationRepository (PushNotificationRepositoryImpl, PushSubscription (PushSubscription), saveSubscriptionImpl)

-- | HTTP-facing subscription type for JSON deserialization
data PushTokenSubscription = PushTokenSubscription
  { tokenAuth :: T.Text,
    tokenP256dh :: T.Text,
    tokenEndpoint :: T.Text,
    tokenExpirationTime :: Maybe UTCTime
  }

data PushController = PushController
  { pushNotificationRepository :: PushNotificationRepositoryImpl
  }

makePushController :: PushNotificationRepositoryImpl -> PushController
makePushController = PushController

subscribe :: PushController -> T.Text -> PushTokenSubscription -> IO ()
subscribe controller userId tokenSub =
  saveSubscriptionImpl
    (pushNotificationRepository controller)
    (PushSubscription userId (tokenEndpoint tokenSub) (tokenAuth tokenSub) (tokenP256dh tokenSub) (tokenExpirationTime tokenSub))
