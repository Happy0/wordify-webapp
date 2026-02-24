module Handler.Notifications where

import Import hiding (Notification (..))
import Data.Aeson ((.=), object, withObject, (.:))
import Data.Time (UTCTime)
import Data.Int (Int64)
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

postNotificationsR :: Handler Value
postNotificationsR = do
  MarkNotificationsReadRequest upTo <- requireCheckJsonBody
  userId <- maybeAuthId >>= maybe notAuthenticated pure
  app <- getYesod
  updatedIds <- liftIO $ markNotificationsAsRead (notificationService app) userId upTo
  returnJson (updatedIds :: [Int64])
