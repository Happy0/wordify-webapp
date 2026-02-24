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
import Controllers.User.Model.ServerUser (ServerUser)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Repository.NotificationRepository
  ( Notification,
    NotificationId,
    NotificationRepository
      ( getNotificationsByUserId,
        markNotificationsAsReadUpTo,
        saveGameInviteNotification
      ),
  )

data NotificationService = forall a. NotificationRepository a => NotificationService a

makeNotificationService :: NotificationRepository a => a -> NotificationService
makeNotificationService = NotificationService

createInviteNotification :: NotificationService -> T.Text -> T.Text -> ServerUser -> IO Notification
createInviteNotification (NotificationService repo) userId lobbyId invitedByUser =
  saveGameInviteNotification repo userId lobbyId invitedByUser

getRecentNotifications :: NotificationService -> T.Text -> IO [Notification]
getRecentNotifications (NotificationService repo) userId =
  fmap (take 5) (getNotificationsByUserId repo userId)

markNotificationsAsRead :: NotificationService -> T.Text -> UTCTime -> IO [NotificationId]
markNotificationsAsRead (NotificationService repo) userId upTo =
  markNotificationsAsReadUpTo repo userId upTo
