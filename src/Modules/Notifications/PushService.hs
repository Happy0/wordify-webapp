{-# LANGUAGE InstanceSigs #-}
module Modules.Notifications.PushService
  ( PushController,
    makePushController,
    sendMoveNotification,
    sendGameStartedNotification,
    sendGameOverNotification,
    createInviteNotification,
    PushTokenSubscription (PushTokenSubscription, tokenAuth, tokenP256dh, tokenEndpoint, tokenExpirationTime),
    pushNotificationRepository,
  )
where

import ClassyPrelude (IO, Maybe (Nothing, Just), Either (..), pure, mapM_, ($), putStr, putStrLn, show, writeTQueue, (.), Functor (fmap), (<$>), Bool (..), maybe)
import Control.Lens ((.~))
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.HTTP.Client (Manager)
import Repository.PushNotificationRepository (PushNotificationRepositoryImpl, PushSubscription (PushSubscription, baseHostName), saveSubscriptionImpl, getSubscriptionsByUserIdImpl, deleteSubscriptionImpl)
import Web.WebPush (VAPIDKeys, PushNotificationError (RecepientEndpointNotFound), sendPushNotification, mkPushNotification, pushMessage, pushTopic, PushTopic (PushTopic))
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
    notificationUrl :: T.Text,
    notificationIcon :: Maybe T.Text
  }

instance A.ToJSON NotificationMessage where
  toJSON :: NotificationMessage -> A.Value
  toJSON (NotificationMessage t u icon) = A.object (["title" A..= ("Wordify" :: T.Text), "body" A..= t, "url" A..= u] ++ maybe [] (\i -> ["icon" A..= i]) icon)

data PushEvent =
  MoveNotification {moveNotificationUserId :: T.Text, moveNotificationGameId :: T.Text}
  | GameStartedNotification {gameStartedNotificationUserId :: T.Text, gameStartedNotificationGameId :: T.Text}
  | GameOverNotification {gameOverNotificationUserId :: T.Text, gameOverNotificationGameId :: T.Text}
  | GameInviteNotification {gameInviteNotificationUserId :: T.Text, gameInviteNotificationGameId :: T.Text}

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

sendMoveNotification :: PushController -> T.Text -> T.Text -> IO ()
sendMoveNotification (PushController _ _ _ workQueue _) userId gameId = atomically (writeTQueue workQueue (MoveNotification userId gameId))

sendGameStartedNotification :: PushController -> T.Text -> T.Text -> IO ()
sendGameStartedNotification (PushController _ _ _ workQueue _) userId gameId = atomically (writeTQueue workQueue (GameStartedNotification userId gameId))

sendGameOverNotification :: PushController -> T.Text -> T.Text -> IO ()
sendGameOverNotification (PushController _ _ _ workQueue _) userId gameId = atomically (writeTQueue workQueue (GameOverNotification userId gameId))

createInviteNotification :: PushController -> T.Text -> T.Text -> IO ()
createInviteNotification (PushController _ _ _ workQueue _) userId gameId = atomically (writeTQueue workQueue (GameInviteNotification userId gameId))

processNotificationEvent :: PushController -> PushEvent -> IO ()
processNotificationEvent pushController event = do
  subscriptions <- getSubscriptionsByUserIdImpl (pushNotificationRepository pushController) userId
  forM_ subscriptions $
    \subscription ->
      let url = T.concat [baseHostName subscription, "/games/", gameId, urlSuffix]
          icon = if hasBoard then Just (T.concat [baseHostName subscription, "/games/", gameId, "/board.svg"]) else Nothing
      in sendNotification pushController subscription message url icon gameId
  where
    (userId, message, gameId, urlSuffix, hasBoard) = case event of
      MoveNotification user gId        -> (user, "It's your move!", gId, "", True)
      GameOverNotification user gId    -> (user, "Your game has ended!", gId, "", True)
      GameStartedNotification user gId -> (user, "Your game has started!", gId, "", True)
      GameInviteNotification user gId  -> (user, "You've been invited to a game!", gId, "/lobby/invite", False)

sendNotification :: PushController -> PushSubscription -> T.Text -> T.Text -> Maybe T.Text -> T.Text -> IO ()
sendNotification controller (PushSubscription _ subEndpoint subAuth subP256dh _ _) notifText notifUrl notifIcon topic =
  case vapidKeys controller of
    Nothing -> putStrLn "vapid keys not configured"
    Just keys -> do
      let baseNotification = mkPushNotification subEndpoint subP256dh subAuth
          notification = (pushTopic .~ Just (PushTopic topic)) . (pushMessage .~ NotificationMessage notifText notifUrl notifIcon) $ baseNotification
      result <- sendPushNotification keys (httpManager controller) notification
      case result of
        Left RecepientEndpointNotFound ->
          deleteSubscriptionImpl (pushNotificationRepository controller) subEndpoint
        Left err -> putStrLn (T.pack (show err))
        Right _ -> pure ()
