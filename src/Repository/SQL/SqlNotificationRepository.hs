module Repository.SQL.SqlNotificationRepository (SqlNotificationRepositoryBackend (SqlNotificationRepositoryBackend)) where

import ClassyPrelude (IO, Maybe (Just, Nothing), ($), (*), fmap, flip, mapM, return)
import Data.Maybe (catMaybes)
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import Database.Persist.Sql
import qualified Model as M
import Repository.NotificationRepository

newtype SqlNotificationRepositoryBackend = SqlNotificationRepositoryBackend (Pool SqlBackend)

instance NotificationRepository SqlNotificationRepositoryBackend where
  getNotificationsByUserId (SqlNotificationRepositoryBackend pool) = getNotificationsByUserIdSQL pool
  saveGameInviteNotification (SqlNotificationRepositoryBackend pool) = saveGameInviteNotificationSQL pool
  markNotificationsAsReadUpTo (SqlNotificationRepositoryBackend pool) = markNotificationsAsReadUpToSQL pool

getNotificationsByUserIdSQL :: Pool SqlBackend -> T.Text -> IO [Notification]
getNotificationsByUserIdSQL pool userId =
  withPool pool $ do
    notifEntities <- selectList [M.NotificationUserId ==. M.UserKey userId] [Desc M.NotificationCreatedAt]
    fmap catMaybes $ mapM notificationFromEntity notifEntities
  where
    notificationFromEntity (Entity notifKey (M.Notification _ createdAt readAt expiresAt notifType)) =
      case notifType of
        "game_invite" -> do
          maybeInvite <- selectFirst [M.InviteNotificationNotificationId ==. notifKey] []
          return $ case maybeInvite of
            Nothing -> Nothing
            Just (Entity _ (M.InviteNotification _ (M.LobbyKey lobbyId) (M.UserKey invitedById))) ->
              Just Notification
                { notificationId = fromSqlKey notifKey,
                  notificationUserId = userId,
                  notificationCreatedAt = createdAt,
                  notificationReadAt = readAt,
                  notificationExpiresAt = expiresAt,
                  notificationDetails = GameInvite (GameInviteDetails lobbyId invitedById)
                }
        _ -> return Nothing

saveGameInviteNotificationSQL :: Pool SqlBackend -> T.Text -> T.Text -> T.Text -> IO ()
saveGameInviteNotificationSQL pool userId lobbyId invitedByUserId = do
  now <- getCurrentTime
  withPool pool $ do
    notifKey <- insert (M.Notification (M.UserKey userId) now Nothing Nothing "game_invite")
    insert_ (M.InviteNotification notifKey (M.LobbyKey lobbyId) (M.UserKey invitedByUserId))

markNotificationsAsReadUpToSQL :: Pool SqlBackend -> T.Text -> UTCTime -> IO ()
markNotificationsAsReadUpToSQL pool userId upTo = do
  now <- getCurrentTime
  let sixMonthsFromNow = addUTCTime (6 * 30 * nominalDay) now
  withPool pool $
    updateWhere
      [ M.NotificationUserId ==. M.UserKey userId,
        M.NotificationCreatedAt <=. upTo
      ]
      [ M.NotificationReadAt =. Just now,
        M.NotificationExpiresAt =. Just sixMonthsFromNow
      ]

withPool = flip runSqlPersistMPool
