module Repository.SQL.SqlNotificationRepository (SqlNotificationRepositoryBackend (SqlNotificationRepositoryBackend)) where

import ClassyPrelude (IO, Maybe (Just, Nothing), map, ($), (*), flip, return)
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import Database.Persist.Sql
import qualified Model as M
import Repository.NotificationRepository
import qualified Controllers.User.Model.ServerUser as SU

newtype SqlNotificationRepositoryBackend = SqlNotificationRepositoryBackend (Pool SqlBackend)

instance NotificationRepository SqlNotificationRepositoryBackend where
  getNotificationsByUserId (SqlNotificationRepositoryBackend pool) = getNotificationsByUserIdSQL pool
  saveGameInviteNotification (SqlNotificationRepositoryBackend pool) = saveGameInviteNotificationSQL pool
  markNotificationsAsReadUpTo (SqlNotificationRepositoryBackend pool) = markNotificationsAsReadUpToSQL pool

getNotificationsByUserIdSQL :: Pool SqlBackend -> T.Text -> IO [Notification]
getNotificationsByUserIdSQL pool userId =
  withPool pool $ do
    results <- rawSql
      "SELECT n.id, n.created_at, n.read_at, n.expires_at, inv.lobby_id, u.ident, u.username FROM notification n JOIN invite_notification inv ON inv.notification_id = n.id JOIN \"user\" u ON u.ident = inv.invited_by WHERE n.user_id = ? AND n.notification_type = 'game_invite' ORDER BY n.created_at DESC"
      [PersistText userId]
    return $ map (rowToNotification userId) results
  where
    rowToNotification uid (Single nId, Single createdAt, Single readAt, Single expiresAt, Single lobbyId, Single inviterIdent, Single inviterUsername) =
      Notification
        { notificationId        = nId
        , notificationUserId    = uid
        , notificationCreatedAt = createdAt
        , notificationReadAt    = readAt
        , notificationExpiresAt = expiresAt
        , notificationDetails   = GameInvite (GameInviteDetails lobbyId (SU.ServerUser inviterIdent inviterUsername))
        }

saveGameInviteNotificationSQL :: Pool SqlBackend -> T.Text -> T.Text -> SU.ServerUser -> IO Notification
saveGameInviteNotificationSQL pool userId lobbyId invitedByUser = do
  now <- getCurrentTime
  withPool pool $ do
    notifKey <- insert (M.Notification (M.UserKey userId) now Nothing Nothing "game_invite")
    insert_ (M.InviteNotification notifKey (M.LobbyKey lobbyId) (M.UserKey (SU.userId invitedByUser)))
    return $ Notification
      { notificationId        = fromSqlKey notifKey
      , notificationUserId    = userId
      , notificationCreatedAt = now
      , notificationReadAt    = Nothing
      , notificationExpiresAt = Nothing
      , notificationDetails   = GameInvite (GameInviteDetails lobbyId invitedByUser)
      }

markNotificationsAsReadUpToSQL :: Pool SqlBackend -> T.Text -> UTCTime -> IO [NotificationId]
markNotificationsAsReadUpToSQL pool userId upTo = do
  now <- getCurrentTime
  let sixMonthsFromNow = addUTCTime (6 * 30 * nominalDay) now
  withPool pool $ do
    keys <- selectKeysList
      [ M.NotificationUserId ==. M.UserKey userId,
        M.NotificationCreatedAt <=. upTo
      ]
      []
    updateWhere
      [ M.NotificationUserId ==. M.UserKey userId,
        M.NotificationCreatedAt <=. upTo
      ]
      [ M.NotificationReadAt =. Just now,
        M.NotificationExpiresAt =. Just sixMonthsFromNow
      ]
    return (map fromSqlKey keys)

withPool = flip runSqlPersistMPool
