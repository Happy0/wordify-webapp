module Repository.SQL.SqlPushNotificationRepository (SqlPushNotificationRepositoryBackend (SqlPushNotificationRepositoryBackend)) where

import ClassyPrelude (IO, Maybe (Nothing), flip, liftIO, map, return, ($), (.))
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Sql
import qualified Model as M
import Repository.PushNotificationRepository (PushNotificationRepository (deleteSubscription, getSubscriptionsByUserId, saveSubscription), PushSubscription (PushSubscription))

newtype SqlPushNotificationRepositoryBackend = SqlPushNotificationRepositoryBackend (Pool SqlBackend)

instance PushNotificationRepository SqlPushNotificationRepositoryBackend where
  saveSubscription (SqlPushNotificationRepositoryBackend pool) = saveSubscriptionImpl pool
  getSubscriptionsByUserId (SqlPushNotificationRepositoryBackend pool) = getSubscriptionsByUserIdImpl pool
  deleteSubscription (SqlPushNotificationRepositoryBackend pool) = deleteSubscriptionImpl pool

saveSubscriptionImpl :: Pool SqlBackend -> PushSubscription -> IO ()
saveSubscriptionImpl pool (PushSubscription usrId endpoint auth p256dh expirationTime) = do
  withPool pool $ do
    _ <- insert (M.PushNotificationSubscription (M.UserKey usrId) endpoint auth p256dh expirationTime)
    return ()

getSubscriptionsByUserIdImpl :: Pool SqlBackend -> T.Text -> IO [PushSubscription]
getSubscriptionsByUserIdImpl pool usrId = do
  entities <- withPool pool $ selectList [M.PushNotificationSubscriptionUserId ==. M.UserKey usrId] []
  return $ map subscriptionFromEntity entities

deleteSubscriptionImpl :: Pool SqlBackend -> T.Text -> IO ()
deleteSubscriptionImpl pool endpoint = do
  withPool pool $ deleteWhere [M.PushNotificationSubscriptionEndpoint ==. endpoint]

subscriptionFromEntity :: Entity M.PushNotificationSubscription -> PushSubscription
subscriptionFromEntity (Entity _ (M.PushNotificationSubscription (M.UserKey usrId) endpoint auth p256dh expirationTime)) =
  PushSubscription usrId endpoint auth p256dh expirationTime

withPool = flip runSqlPersistMPool
