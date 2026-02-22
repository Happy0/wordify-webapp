{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repository.SQL.Migrations (runMigrations) where

import Control.Monad (return)
import Control.Monad.IO.Class (MonadIO)
import ClassyPrelude.Yesod (SqlPersistT)

-- | Run manual migrations that must execute before Persistent's migrateAll.
runMigrations :: MonadIO m => SqlPersistT m ()
runMigrations = return ()
