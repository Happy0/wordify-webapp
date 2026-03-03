{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repository.SQL.Migrations (runMigrations) where

import Control.Monad (mapM_, return)
import Control.Monad.IO.Class (MonadIO)
import ClassyPrelude.Yesod (SqlPersistT)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist.Sql (rawExecute)

-- | Run manual migrations that must execute before Persistent's migrateAll.
runMigrations :: MonadIO m => SqlPersistT m ()
runMigrations = mapM_ fixCol timestampColumns
  where
    -- Fix ISO 8601 timestamps (e.g. "2026-03-01T18:08:17.512916098Z") across all
    -- tables. persistent-sqlite 2.13.x expects space-separated UTCTime format
    -- "2026-03-01 18:08:17.512916098"; old data stored with T/Z separators fails to parse.
    fixCol (tbl, col) =
      rawExecute
        ("UPDATE " <> tbl <> " SET " <> col <> " = REPLACE(REPLACE(" <> col <> ", 'T', ' '), 'Z', '') WHERE " <> col <> " LIKE '%T%'")
        []

    timestampColumns =
      [ ("game",                       "created_at")
      , ("game",                       "finished_at")
      , ("game",                       "last_move_made_at")
      , ("chat_message",               "created_at")
      , ("player",                     "last_active")
      , ("lobby",                      "created_at")
      , ("lobby_player",               "last_active")
      , ("game_definition",            "created_at")
      , ("push_notification_subscription", "expiration_time")
      , ("notification",               "created_at")
      , ("notification",               "read_at")
      , ("notification",               "expires_at")
      ]
