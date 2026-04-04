module Handler.Common.ClientNotificationPresentation
  ( notificationsForUser
  , sendNotificationUpdate
  , nextNotifUpdateFromUserChan
  , notificationsOnlyWebsocketHandler
  , notificationsWebSocketHandler
  ) where

import Import hiding (Notification (..))
import Control.Concurrent (threadDelay)
import Data.Aeson ((.=), object, encode)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import InactivityTracker (withTrackWebsocketActivity)
import Modules.Notifications.Api (getRecentNotifications)
import Modules.UserEvent.Api (subscribeToUserChannel)
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

-- | Higher-order WebSocket handler that:
-- 1. Opens the user event channel
-- 2. Sends the initial notification state to the client (after the channel is
--    open so no events can be missed between fetching and subscribing)
-- 3. Invokes the given callback with the channel so the caller can run
--    the rest of the WebSocket session
notificationsWebSocketHandler
  :: App
  -> Text                                              -- userId
  -> (TChan UserEvent -> WebSocketsT Handler ())       -- callback
  -> WebSocketsT Handler ()
notificationsWebSocketHandler app userId callback = do
  connection <- ask
  withRunInIO $ \runInIO ->
    runResourceT $ do
      (_, eitherChan) <- subscribeToUserChannel (userEventService app) userId
      case eitherChan of
        Left _ -> liftIO $ C.sendCloseCode connection 1011 ("Unexpectedly failed to subscribe to notifications." :: Text)
        Right userEventChan -> liftIO $ do
          notifs <- notificationsForUser app userId
          C.sendTextData connection $ encode $ object
            [ "command" .= ("initialNotifications" :: Text)
            , "payload" .= notifs
            ]
          runInIO (callback userEventChan)

-- | WebSocket handler that forwards notification updates to the client.
-- For use by endpoints that serve no other websocket messages.
notificationsOnlyWebsocketHandler :: App -> Text -> WebSocketsT Handler ()
notificationsOnlyWebsocketHandler app userId =
  notificationsWebSocketHandler app userId $ \userEventChan -> do
    connection <- ask
    liftIO $ withTrackWebsocketActivity (inactivityTracker app) $
      race_
        (forever $ do
          update <- atomically (nextNotifUpdateFromUserChan userEventChan)
          sendNotificationUpdate connection update)
        (forever $ C.sendPing connection ("hello~" :: Text) >> threadDelay 60000000)

-- | Reads from the user event channel, skipping (and discarding) non-notification
-- events until a NotificationsChanged event is found.
nextNotifUpdateFromUserChan :: TChan UserEvent -> STM NotificationUpdate
nextNotifUpdateFromUserChan chan = do
  event <- readTChan chan
  case event of
    NotificationsChanged u -> return u
    _                      -> nextNotifUpdateFromUserChan chan
