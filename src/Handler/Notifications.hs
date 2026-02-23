module Handler.Notifications where

import Import hiding (Notification (..))
import Data.Aeson ((.=), object, withObject, (.:))
import Data.Time (UTCTime)
import Modules.Notifications.Api (getRecentNotifications, markNotificationsAsRead)
import Repository.NotificationRepository (Notification (..), NotificationDetails (..), GameInviteDetails (..))

-- | POST /api/notifications request body
data MarkNotificationsReadRequest = MarkNotificationsReadRequest
  { beforeDateTimeInclusive :: UTCTime
  }

instance FromJSON MarkNotificationsReadRequest where
  parseJSON = withObject "MarkNotificationsReadRequest" $ \obj ->
    MarkNotificationsReadRequest <$> obj .: "beforeDateTimeInclusive"

getNotificationsR :: Handler Value
getNotificationsR = do
  userId <- maybeAuthId >>= maybe notAuthenticated pure
  app <- getYesod
  notifications <- liftIO $ getRecentNotifications (notificationService app) userId
  returnJson (map notificationToJson notifications)

postNotificationsR :: Handler ()
postNotificationsR = do
  MarkNotificationsReadRequest upTo <- requireCheckJsonBody
  userId <- maybeAuthId >>= maybe notAuthenticated pure
  app <- getYesod
  liftIO $ markNotificationsAsRead (notificationService app) userId upTo

notificationToJson :: Notification -> Value
notificationToJson n = object
  [ "id"        .= notificationId n
  , "createdAt" .= notificationCreatedAt n
  , "readAt"    .= notificationReadAt n
  , "expiresAt" .= notificationExpiresAt n
  , "details"   .= detailsToJson (notificationDetails n)
  ]

detailsToJson :: NotificationDetails -> Value
detailsToJson (GameInvite GameInviteDetails {..}) = object
  [ "type"            .= ("game_invite" :: Text)
  , "lobbyId"         .= inviteLobbyId
  , "invitedByUserId" .= invitedByUserId
  ]
