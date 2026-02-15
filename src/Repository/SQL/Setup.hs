{-# LANGUAGE OverloadedStrings #-}

module Repository.SQL.Setup (runSetup) where

import Database.Persist.Sqlite (rawExecute)
import Database.Persist.Sql (SqlPersistT)
import Control.Monad.IO.Class (MonadIO)

-- | Run additional database setup steps (indexes, etc.) that cannot be
--   expressed in the Persistent models file.
runSetup :: MonadIO m => SqlPersistT m ()
runSetup = do
  -- Case-insensitive unique index on username so that e.g. "Alice" and "alice"
  -- are treated as the same username, while preserving the user's chosen casing.
  rawExecute
    "CREATE UNIQUE INDEX IF NOT EXISTS unique_username_nocase ON \"user\"(username COLLATE NOCASE)"
    []

  -- Index on chat_message for efficient lookup by game and ordering by creation time.
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_chat_message_game_created_at ON chat_message(game, created_at)"
    []
