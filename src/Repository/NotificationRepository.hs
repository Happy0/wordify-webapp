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
import Controllers.User.Model.ServerUser (ServerUser)

type NotificationId = Int64

data GameInviteDetails = GameInviteDetails
  { inviteLobbyId :: T.Text,
    invitedByUser :: ServerUser
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
  -- | Save a game invite notification (userId, lobbyId, invitedByUser), returning the saved Notification
  saveGameInviteNotification :: a -> T.Text -> T.Text -> ServerUser -> IO Notification
  -- | Mark all notifications for a user created at or before the given time as read, returning the IDs of the updated notifications
  markNotificationsAsReadUpTo :: a -> T.Text -> UTCTime -> IO [NotificationId]
