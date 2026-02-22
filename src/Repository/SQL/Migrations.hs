{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repository.SQL.Migrations (runMigrations) where

import ClassyPrelude (not, null, ($), (<>))
import Control.Monad (when)
import Data.Text (Text)
import Database.Persist.Sqlite (rawExecute)
import Database.Persist.Sql (Single, SqlPersistT, rawSql)
import Control.Monad.IO.Class (MonadIO)

-- | Run manual migrations that must execute before Persistent's migrateAll.
runMigrations :: MonadIO m => SqlPersistT m ()
runMigrations = do
  migrateMovePrimaryKey

-- | Migrate the move table from a surrogate id primary key to a composite
--   (game, move_number) natural primary key, with game as a foreign key
--   referencing the game table.
--
--   Detected by the presence of the old 'id' column; safe to call on an
--   already-migrated database.
migrateMovePrimaryKey :: MonadIO m => SqlPersistT m ()
migrateMovePrimaryKey = do
  (cols :: [Single Text]) <-
    rawSql
      "SELECT name FROM pragma_table_info('move') WHERE name = 'id'"
      []
  when (not (null cols)) $ do
    -- Rebuild the table with the new schema. SQLite does not support ALTER
    -- TABLE for primary key changes, so we create a replacement table, copy
    -- the data, drop the old table, and rename.
    rawExecute
      (  "CREATE TABLE move_migration_temp"
      <> "( game TEXT NOT NULL REFERENCES \"game\"(game_id)"
      <> ", move_number INTEGER NOT NULL"
      <> ", tiles VARCHAR"
      <> ", start_x INTEGER"
      <> ", start_y INTEGER"
      <> ", is_horizontal INTEGER"
      <> ", PRIMARY KEY (game, move_number)"
      <> ")"
      )
      []
    -- Copy one row per (game, move_number) pair, keeping the earliest insert
    -- (MIN(id)) to discard any duplicates that were created without a unique
    -- constraint in the old schema.
    rawExecute
      (  "INSERT INTO move_migration_temp"
      <> " (game, move_number, tiles, start_x, start_y, is_horizontal)"
      <> " SELECT game, move_number, tiles, start_x, start_y, is_horizontal"
      <> " FROM move"
      <> " WHERE id IN (SELECT MIN(id) FROM move GROUP BY game, move_number)"
      )
      []
    rawExecute "DROP TABLE move" []
    rawExecute "ALTER TABLE move_migration_temp RENAME TO move" []
