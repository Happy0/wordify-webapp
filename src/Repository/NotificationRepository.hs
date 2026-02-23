module Repository.NotificationRepository
  ( NotificationRepository (getNotificationsByUserId, saveGameInviteNotification, markNotificationsAsReadUpTo),
    Notification (..),
    NotificationDetails (..),
    GameInviteDetails (..),
    NotificationId,
  )
where

import ClassyPrelude (IO, Maybe)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Int (Int64)

type NotificationId = Int64

data GameInviteDetails = GameInviteDetails
  { inviteLobbyId :: T.Text,
    invitedByUserId :: T.Text
  }

data NotificationDetails
  = GameInvite GameInviteDetails

data Notification = Notification
  { notificationId :: NotificationId,
    notificationUserId :: T.Text,
    notificationCreatedAt :: UTCTime,
    notificationReadAt :: Maybe UTCTime,
    notificationExpiresAt :: Maybe UTCTime,
    notificationDetails :: NotificationDetails
  }

class NotificationRepository a where
  -- | Get all notifications for a user in descending order of creation time
  getNotificationsByUserId :: a -> T.Text -> IO [Notification]
  -- | Save a game invite notification (userId, lobbyId, invitedByUserId)
  saveGameInviteNotification :: a -> T.Text -> T.Text -> T.Text -> IO ()
  -- | Mark all notifications for a user created at or before the given time as read
  markNotificationsAsReadUpTo :: a -> T.Text -> UTCTime -> IO ()
