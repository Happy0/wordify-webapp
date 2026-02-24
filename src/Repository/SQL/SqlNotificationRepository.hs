module Repository.SQL.SqlNotificationRepository (SqlNotificationRepositoryBackend (SqlNotificationRepositoryBackend)) where

import ClassyPrelude (IO, Maybe (Just, Nothing), map, ($), (*), flip, return)
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import Database.Persist.Sql
import qualified Model as M
import Repository.NotificationRepository
import Controllers.User.Model.ServerUser (ServerUser (ServerUser))

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
        , notificationDetails   = GameInvite (GameInviteDetails lobbyId (ServerUser inviterIdent inviterUsername))
        }

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
