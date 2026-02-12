module Repository.PushNotificationRepository
  ( toPushNotificationRepositoryImpl,
    PushNotificationRepository (saveSubscription, getSubscriptionsByUserId, deleteSubscription),
    PushNotificationRepositoryImpl,
    saveSubscriptionImpl,
    getSubscriptionsByUserIdImpl,
    deleteSubscriptionImpl,
    PushSubscription (PushSubscription, baseHostName),
  )
where

import ClassyPrelude (IO, Maybe)
import qualified Data.Text as T
import Data.Time (UTCTime)

data PushSubscription = PushSubscription
  { userId :: T.Text,
    endpoint :: T.Text,
    auth :: T.Text,
    p256dh :: T.Text,
    expirationTime :: Maybe UTCTime,
    baseHostName :: T.Text
  }

class PushNotificationRepository a where
  saveSubscription :: a -> PushSubscription -> IO ()
  getSubscriptionsByUserId :: a -> T.Text -> IO [PushSubscription]
  deleteSubscription :: a -> T.Text -> IO ()

data PushNotificationRepositoryImpl = PushNotificationRepositoryImpl
  { savePushSubscription :: PushSubscription -> IO (),
    getPushSubscriptionsByUserId :: T.Text -> IO [PushSubscription],
    deletePushSubscription :: T.Text -> IO ()
  }

toPushNotificationRepositoryImpl :: (PushNotificationRepository a) => a -> PushNotificationRepositoryImpl
toPushNotificationRepositoryImpl repository =
  PushNotificationRepositoryImpl
    { savePushSubscription = saveSubscription repository,
      getPushSubscriptionsByUserId = getSubscriptionsByUserId repository,
      deletePushSubscription = deleteSubscription repository
    }

saveSubscriptionImpl :: PushNotificationRepositoryImpl -> PushSubscription -> IO ()
saveSubscriptionImpl
  (PushNotificationRepositoryImpl savePushSubscription _ _) =
    savePushSubscription

getSubscriptionsByUserIdImpl :: PushNotificationRepositoryImpl -> T.Text -> IO [PushSubscription]
getSubscriptionsByUserIdImpl
  (PushNotificationRepositoryImpl _ getPushSubscriptionsByUserId _) =
    getPushSubscriptionsByUserId

deleteSubscriptionImpl :: PushNotificationRepositoryImpl -> T.Text -> IO ()
deleteSubscriptionImpl
  (PushNotificationRepositoryImpl _ _ deletePushSubscription) =
    deletePushSubscription
