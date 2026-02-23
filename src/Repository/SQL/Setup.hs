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

  -- Index on chat_message for efficient lookup by chatId and ordering by creation time.
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_chat_message_chat_id_created_at ON chat_message(chat_id, created_at)"
    []

  -- Index on player for efficient lookup by game ID.
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_player_game_id ON player(game_id)"
    []

  -- Index on lobby_player for efficient lookup by game.
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_lobby_player_game ON lobby_player(game)"
    []

  -- Index on game_definition for efficient lookup by game ID.
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_game_definition_game_id ON game_definition(game_id)"
    []

  -- Composite index on notification for efficient lookup by user and ordering by creation time.
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_notification_user_id_created_at ON notification(user_id, created_at)"
    []

  -- Index on notification for efficient querying of notifications expiring before a given date.
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_notification_expires_at ON notification(expires_at)"
    []
