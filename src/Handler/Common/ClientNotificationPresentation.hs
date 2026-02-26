module Handler.Common.ClientNotificationPresentation
  ( notificationsForUser
  , sendNotificationUpdate
  , nextNotifUpdateFromUserChan
  , withUserEventChan
  , notificationsWebSocketHandler
  ) where

import Import hiding (Notification (..))
import Control.Concurrent (threadDelay)
import Data.Aeson ((.=), object, encode)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import InactivityTracker (withTrackWebsocketActivity)
import Modules.Notifications.Api (getRecentNotifications)
import Modules.UserEvent.Api (UserEventService, subscribeToUserChannel)
import Repository.NotificationRepository (Notification (..), NotificationDetails (..), GameInviteDetails (..))
import qualified Controllers.User.Model.ServerUser as SU
import Controllers.Game.Model.UserEventSubscription (UserEvent (..), NotificationUpdate (..))
import qualified Network.WebSockets.Connection as C
import Yesod.WebSockets (WebSocketsT)

notificationsForUser :: App -> Text -> IO [Value]
notificationsForUser app userId = do
  notifs <- getRecentNotifications (notificationService app) userId
  return $ map notificationToClientJson notifs

notificationToClientJson :: Notification -> Value
notificationToClientJson notif = object $
  [ "notificationId"        .= show (notificationId notif)
  , "notificationCreatedAt" .= utcToMs (notificationCreatedAt notif)
  , "notificationDetails"   .= notifDetailsToClientJson (notificationDetails notif)
  , "url"                   .= notifUrl (notificationDetails notif)
  ] ++
  case notificationReadAt notif of
    Nothing -> []
    Just t  -> ["notificationReadAt" .= utcToMs t]

utcToMs :: UTCTime -> Integer
utcToMs = round . (* 1000) . utcTimeToPOSIXSeconds

notifDetailsToClientJson :: NotificationDetails -> Value
notifDetailsToClientJson (GameInvite GameInviteDetails {..}) = object
  [ "type"           .= ("gameInvite" :: Text)
  , "inviteFromUser" .= fromMaybe (SU.userId invitedByUser) (SU.username invitedByUser)
  , "gameLobbyId"    .= inviteLobbyId
  ]

notifUrl :: NotificationDetails -> Text
notifUrl (GameInvite GameInviteDetails {..}) = "/games/" <> inviteLobbyId <> "/lobby/invite"

sendNotificationUpdate :: C.Connection -> NotificationUpdate -> IO ()
sendNotificationUpdate connection (NotificationAdded notification) =
  C.sendTextData connection $ encode $ object
    [ "command" .= ("notificationAdded" :: Text)
    , "payload" .= notificationToClientJson notification
    ]
sendNotificationUpdate connection (NotificationsRead ids) =
  C.sendTextData connection $ encode $ object
    [ "command" .= ("notificationsRead" :: Text)
    , "payload" .= ids
    ]

-- | WebSocket handler that forwards notification updates to the client.
-- For use by endpoints that serve no other websocket messages.
notificationsWebSocketHandler :: App -> Text -> WebSocketsT Handler ()
notificationsWebSocketHandler app userId = do
  connection <- ask
  liftIO $
    withTrackWebsocketActivity (inactivityTracker app) $ runResourceT $
      withUserEventChan (userEventService app) userId $ \userEventChan ->
        race_
          (forever $ do
            update <- atomically (nextNotifUpdateFromUserChan userEventChan)
            sendNotificationUpdate connection update)
          (forever $ C.sendPing connection ("hello~" :: Text) >> threadDelay 60000000)

-- | Subscribes to the user event channel and passes it to the given action.
-- If the subscription fails, returns () without calling the action.
withUserEventChan :: MonadResource m => UserEventService -> Text -> (TChan UserEvent -> IO ()) -> m ()
withUserEventChan service userId action = do
  (_, eitherChan) <- subscribeToUserChannel service userId
  case eitherChan of
    Left _    -> return ()
    Right chan -> liftIO $ action chan

-- | Reads from the user event channel, skipping (and discarding) non-notification
-- events until a NotificationsChanged event is found.
nextNotifUpdateFromUserChan :: TChan UserEvent -> STM NotificationUpdate
nextNotifUpdateFromUserChan chan = do
  event <- readTChan chan
  case event of
    NotificationsChanged u -> return u
    _                      -> nextNotifUpdateFromUserChan chan
