{-# LANGUAGE ExistentialQuantification #-}
module Modules.Notifications.NotificationService
  ( NotificationService,
    makeNotificationService,
    createInviteNotification,
    getRecentNotifications,
    markNotificationsAsRead,
  )
where

import ClassyPrelude (IO, fmap, take)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Repository.NotificationRepository
  ( Notification,
    NotificationRepository
      ( getNotificationsByUserId,
        markNotificationsAsReadUpTo,
        saveGameInviteNotification
      ),
  )

data NotificationService = forall a. NotificationRepository a => NotificationService a

makeNotificationService :: NotificationRepository a => a -> NotificationService
makeNotificationService = NotificationService

createInviteNotification :: NotificationService -> T.Text -> T.Text -> T.Text -> IO ()
createInviteNotification (NotificationService repo) userId lobbyId invitedByUserId =
  saveGameInviteNotification repo userId lobbyId invitedByUserId

getRecentNotifications :: NotificationService -> T.Text -> IO [Notification]
getRecentNotifications (NotificationService repo) userId =
  fmap (take 5) (getNotificationsByUserId repo userId)

markNotificationsAsRead :: NotificationService -> T.Text -> UTCTime -> IO ()
markNotificationsAsRead (NotificationService repo) userId upTo =
  markNotificationsAsReadUpTo repo userId upTo
