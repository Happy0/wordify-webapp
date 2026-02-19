{-# LANGUAGE OverloadedStrings #-}

module Repository.SQL.Setup (runSetup) where

import Control.Monad (when)
import Data.Text (Text)
import Database.Persist.Sqlite (rawExecute)
import Database.Persist.Sql (SqlPersistT, Single, rawSql)
import Control.Monad.IO.Class (MonadIO)
import ClassyPrelude (not, ($))
import ClassyPrelude (null)

-- | Run additional database setup steps (indexes, etc.) that cannot be
--   expressed in the Persistent models file.
runSetup :: MonadIO m => SqlPersistT m ()
runSetup = do
  -- Case-insensitive unique index on username so that e.g. "Alice" and "alice"
  -- are treated as the same username, while preserving the user's chosen casing.
  rawExecute
    "CREATE UNIQUE INDEX IF NOT EXISTS unique_username_nocase ON \"user\"(username COLLATE NOCASE)"
    []

  -- Drop the old game-named index if it exists (left over from before the column rename).
  rawExecute
    "DROP INDEX IF EXISTS idx_chat_message_game_created_at"
    []

  -- Index on chat_message for efficient lookup by chatId and ordering by creation time.
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_chat_message_chat_id_created_at ON chat_message(chat_id, created_at)"
    []
