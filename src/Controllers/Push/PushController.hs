{-# LANGUAGE InstanceSigs #-}
module Controllers.Push.PushController
  ( PushController,
    makePushController,
    subscribe,
    sendMoveNotification,
    sendGameStartedNotification,
    sendGameOverNotification,
    PushTokenSubscription (PushTokenSubscription, tokenAuth, tokenP256dh, tokenEndpoint, tokenExpirationTime),
  )
where

import ClassyPrelude (IO, Maybe (Nothing, Just), Either (..), pure, undefined, mapM_, ($), putStr, putStrLn, show, writeTQueue, (.), Functor (fmap), (<$>))
import Control.Lens ((.~))
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.HTTP.Client (Manager)
import Repository.PushNotificationRepository (PushNotificationRepositoryImpl, PushSubscription (PushSubscription), saveSubscriptionImpl, getSubscriptionsByUserIdImpl, deleteSubscriptionImpl)
import Web.WebPush (VAPIDKeys, PushNotificationError (RecepientEndpointNotFound), sendPushNotification, mkPushNotification, pushMessage)
import Controllers.User.Model.AuthUser (AuthUser)
import Control.Monad (forM_, replicateM)
import Data.List ((++))
import Control.Concurrent.STM (TQueue, readTQueue)
import ClassyPrelude (atomically, newTQueueIO, (-))
import Control.Concurrent (forkIO)
import ClassyPrelude (void, Int, catMaybes)
import Control.Monad (forever)
import ClassyPrelude (tryReadTQueue, forConcurrently_, mapConcurrently_)
import Control.Monad (Monad)

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
  toJSON :: NotificationMessage -> A.Value
  toJSON (NotificationMessage t u) = A.object ["title" A..= ("Wordify" :: T.Text), "body" A..= t, "url" A..= u]

data PushEvent =
  MoveNotification {moveNotificationUserId :: T.Text, moveNotificationGameId :: T.Text}
  | GameStartedNotification {gameStartedNotificationUserId :: T.Text, gameStartedNotificationGameId :: T.Text}
  | GameOverNotification { gameOverNotificationUserId :: T.Text, gameOverNotificationGameId :: T.Text}

data PushController = PushController
  { pushNotificationRepository :: PushNotificationRepositoryImpl,
    vapidKeys :: Maybe VAPIDKeys,
    httpManager :: Manager,
    workQueue :: TQueue PushEvent,
    additionalConcurrentPushes :: Int
  }

makePushController :: PushNotificationRepositoryImpl -> Maybe VAPIDKeys -> Manager -> IO PushController
makePushController pushNotificationRepo vapidKeys httpManager = do
  queue <- newTQueueIO
  let controller = PushController pushNotificationRepo vapidKeys httpManager queue 10
  _ <- processQueueBackground controller
  pure controller

processQueueBackground :: PushController -> IO ()
processQueueBackground pushController = do
  _ <- forkIO (processQueueLoop pushController)
  pure ()

processQueueLoop :: PushController -> IO ()
processQueueLoop pushController = forever $ do
  let queue = workQueue pushController
  nextEvent <- atomically (readTQueue queue)
  additionalAvailableEvents <- replicateWhileJust (additionalConcurrentPushes pushController) (atomically (tryReadTQueue queue))
  mapConcurrently_ (processNotificationEvent pushController) (nextEvent : additionalAvailableEvents)
  where
    replicateWhileJust :: Monad m => Int -> m (Maybe a) -> m [a]
    replicateWhileJust 0 _ = pure []
    replicateWhileJust n action = do
      result <- action
      case result of
        Nothing -> pure []
        Just x  -> (x :) <$> replicateWhileJust (n - 1) action

subscribe :: PushController -> T.Text -> PushTokenSubscription -> IO ()
subscribe controller userId tokenSub =
  saveSubscriptionImpl
    (pushNotificationRepository controller)
    (PushSubscription userId (tokenEndpoint tokenSub) (tokenAuth tokenSub) (tokenP256dh tokenSub) (tokenExpirationTime tokenSub))

sendMoveNotification :: PushController -> T.Text -> T.Text -> IO ()
sendMoveNotification pushController@(PushController pushNotificationRepository _ _ workQueue _) userId gameId = atomically (writeTQueue workQueue (MoveNotification userId gameId))

sendGameStartedNotification :: PushController -> T.Text -> T.Text -> IO ()
sendGameStartedNotification pushController@(PushController pushNotificationRepository _ _ workQueue _) userId gameId = atomically (writeTQueue workQueue (GameStartedNotification userId gameId))

sendGameOverNotification :: PushController -> T.Text -> T.Text -> IO ()
sendGameOverNotification pushController@(PushController pushNotificationRepository _ _ workQueue _) userId gameId = atomically (writeTQueue workQueue (GameOverNotification userId gameId))

processNotificationEvent :: PushController -> PushEvent -> IO ()
processNotificationEvent pushController event = do
  subscriptions <- getSubscriptionsByUserIdImpl (pushNotificationRepository pushController) userId
  forM_ subscriptions $
    -- TODO: base URL by configuration
    \subscription -> sendNotification pushController subscription message url
  where
    (userId, message, url) = case event of
      MoveNotification user gameId -> (user, "It's your move!", T.concat ["https://wordify.gordo.life/games/", gameId])
      GameOverNotification user gameId -> (user, "Your game has ended!", T.concat ["https://wordify.gordo.life/games/", gameId])
      GameStartedNotification user gameId -> (user, "Your game has started!", T.concat ["https://wordify.gordo.life/games/", gameId])

sendNotification :: PushController -> PushSubscription -> T.Text -> T.Text -> IO ()
sendNotification controller (PushSubscription _ subEndpoint subAuth subP256dh _) notifText notifUrl =
  case vapidKeys controller of
    Nothing -> putStrLn "vapid keys not configured"
    Just keys -> do
      let baseNotification = mkPushNotification subEndpoint subP256dh subAuth
          notification = (pushMessage .~ NotificationMessage notifText notifUrl) baseNotification
      result <- sendPushNotification keys (httpManager controller) notification
      case result of
        Left RecepientEndpointNotFound ->
          deleteSubscriptionImpl (pushNotificationRepository controller) subEndpoint
        Left err -> putStrLn (T.pack (show err))
        Right _ -> pure ()
