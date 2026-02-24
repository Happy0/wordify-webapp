module Handler.Notifications where

import Import hiding (Notification (..))
import Data.Aeson ((.=), object, withObject, (.:))
import Data.Time (UTCTime)
import Modules.Notifications.Api (markNotificationsAsRead)
import Handler.Common.ClientNotificationPresentation (notificationsForUser)

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
  notifs <- liftIO $ notificationsForUser app userId
  returnJson notifs

postNotificationsR :: Handler ()
postNotificationsR = do
  MarkNotificationsReadRequest upTo <- requireCheckJsonBody
  userId <- maybeAuthId >>= maybe notAuthenticated pure
  app <- getYesod
  liftIO $ markNotificationsAsRead (notificationService app) userId upTo
