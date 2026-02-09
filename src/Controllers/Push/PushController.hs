module Controllers.Push.PushController
  ( PushController,
    makePushController,
    subscribe,
    sendMoveNotification,
    PushTokenSubscription (PushTokenSubscription, tokenAuth, tokenP256dh, tokenEndpoint, tokenExpirationTime),
  )
where

import ClassyPrelude (IO, Maybe (Nothing, Just), Either (..), pure, fmap, undefined, mapM_, ($), putStr, putStrLn, show)
import Control.Lens ((.~))
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.HTTP.Client (Manager)
import Repository.PushNotificationRepository (PushNotificationRepositoryImpl, PushSubscription (PushSubscription), saveSubscriptionImpl, getSubscriptionsByUserIdImpl)
import Web.WebPush (VAPIDKeys, PushNotificationError, sendPushNotification, mkPushNotification, pushMessage)
import Controllers.User.Model.AuthUser (AuthUser)
import Control.Monad (forM_)
import Data.List ((++))

-- | HTTP-facing subscription type for JSON deserialization
data PushTokenSubscription = PushTokenSubscription
  { tokenAuth :: T.Text,
    tokenP256dh :: T.Text,
    tokenEndpoint :: T.Text,
    tokenExpirationTime :: Maybe UTCTime
  }

-- | Notification message payload sent to the browser
data NotificationMessage = NotificationMessage
  { notificationText :: T.Text,
    notificationUrl :: T.Text
  }

instance A.ToJSON NotificationMessage where
  toJSON (NotificationMessage t u) = A.object ["text" A..= t, "url" A..= u]

data PushController = PushController
  { pushNotificationRepository :: PushNotificationRepositoryImpl,
    vapidKeys :: Maybe VAPIDKeys,
    httpManager :: Manager
  }

makePushController :: PushNotificationRepositoryImpl -> Maybe VAPIDKeys -> Manager -> PushController
makePushController = PushController

subscribe :: PushController -> T.Text -> PushTokenSubscription -> IO ()
subscribe controller userId tokenSub =
  saveSubscriptionImpl
    (pushNotificationRepository controller)
    (PushSubscription userId (tokenEndpoint tokenSub) (tokenAuth tokenSub) (tokenP256dh tokenSub) (tokenExpirationTime tokenSub))

sendMoveNotification :: PushController -> T.Text -> T.Text -> IO ()
sendMoveNotification pushController@(PushController pushNotificationRepository _ _) userId gameId = do
  subscriptions <- getSubscriptionsByUserIdImpl pushNotificationRepository userId
  forM_ subscriptions $
    -- TODO: base URL by configuration
    \subscription -> do
      result <- sendNotification pushController subscription "It's your move!" (T.concat ["https://wordify.gordo.life/games/", gameId])

      case result of 
        (Just (Left err)) -> putStrLn (T.pack (show err))
        Nothing -> putStrLn "vapid keys not configured"
        _ -> pure ()

sendNotification :: PushController -> PushSubscription -> T.Text -> T.Text -> IO (Maybe (Either PushNotificationError ()))
sendNotification controller (PushSubscription _ subEndpoint subAuth subP256dh _) notifText notifUrl =
  case vapidKeys controller of
    Nothing -> pure Nothing
    Just keys ->
      let baseNotification = mkPushNotification subEndpoint subP256dh subAuth
          notification = (pushMessage .~ NotificationMessage notifText notifUrl) baseNotification
      in fmap Just (sendPushNotification keys (httpManager controller) notification)
