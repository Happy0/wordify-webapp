module Migrations.PushNotificationHostNameMigration (runPushNotificationHostNameMigration) where

    import qualified Data.Text as T
    import ClassyPrelude (IO, Maybe(Just,Nothing), pure, flip, ($), mapM_, putStrLn, (==))
    import Database.Persist.Sql
    import qualified Model as M
    import Database.Persist (Entity(..))
    import Control.Exception (catch, SomeException)

    runPushNotificationHostNameMigration :: ConnectionPool -> Maybe T.Text -> IO ()
    runPushNotificationHostNameMigration pool maybeAppRoot = do
        putStrLn "Running push notification host name migration..."
        ensureBaseHostNameColumn pool
        subscriptions <- withPool pool $ selectList [] [] :: IO [Entity M.PushNotificationSubscription]
        mapM_ (migrateSubscription pool baseHostName) subscriptions
      where
        baseHostName = case maybeAppRoot of
            Just root -> root
            Nothing -> "http://127.0.0.1:4000"

    ensureBaseHostNameColumn :: ConnectionPool -> IO ()
    ensureBaseHostNameColumn pool =
        withPool pool (rawExecute
            "ALTER TABLE push_notification_subscription ADD COLUMN base_host_name VARCHAR NOT NULL DEFAULT 'http://127.0.0.1:4000'"
            []) `catch` handleAlreadyExists
      where
        handleAlreadyExists :: SomeException -> IO ()
        handleAlreadyExists _ = putStrLn "base_host_name column already exists, skipping."

    migrateSubscription :: ConnectionPool -> T.Text -> Entity M.PushNotificationSubscription -> IO ()
    migrateSubscription pool baseHostName (Entity key subscription) =
        if M.pushNotificationSubscriptionBaseHostName subscription == "http://127.0.0.1:4000"
            then withPool pool $ update key [M.PushNotificationSubscriptionBaseHostName =. baseHostName]
            else pure ()

    withPool = flip runSqlPersistMPool
