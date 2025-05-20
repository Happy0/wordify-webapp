module Migrations.DatabaseTileListFormatMigration (runTileListMigration) where

    import qualified Data.Text as T
    import ClassyPrelude (notElem, Bool, IO, undefined, (>>), Maybe(Just,Nothing), pure, flip, ($), mapM_, putStrLn)
    import Database.Persist.Sql
    import Control.Monad (when)
    import qualified Model as M
    import Database.Persist (Entity)
    import Database.Sqlite (Connection)
    
    runTileListMigration :: ConnectionPool -> IO ()
    runTileListMigration pool = do 
        putStrLn "Running tile list migration..."
        migrateMoves pool >> migrateLobbies pool >> migrateGames pool

    migrateMoves :: ConnectionPool -> IO ()
    migrateMoves pool = do
        moves <- withPool pool $ selectList [] [] :: IO [Entity M.Move]
        mapM_ (migrateMove pool) moves

    migrateLobbies :: ConnectionPool -> IO ()
    migrateLobbies pool = do
        lobbies <- withPool pool $ selectList [] [] :: IO [Entity M.Lobby]
        mapM_ (migrateLobby pool) lobbies

    migrateGames :: ConnectionPool -> IO ()
    migrateGames pool = do
        games <- withPool pool $ selectList [] [] :: IO [Entity M.Game]
        mapM_ (migrateGame pool) games

    migrateMove :: ConnectionPool -> Entity M.Move -> IO ()
    migrateMove pool (Entity _ (M.Move _ _ Nothing _ _ _)) = pure ()
    migrateMove pool (Entity key (M.Move _ _ (Just tileList) _ _ _)) = do
        when (isOldTileListFormat tileList) $ do
            let newTileList = updateTileListFormat tileList
            withPool pool $ update key [M.MoveTiles =. Just newTileList]

    migrateLobby :: ConnectionPool -> Entity M.Lobby -> IO ()
    migrateLobby pool (Entity key (M.Lobby gameId letterBag _ _ _ _)) = do
        when (isOldTileListFormat letterBag) $ do
            let newLetterBag = updateTileListFormat letterBag
            withPool pool $ update key [M.LobbyOriginalLetterBag =. newLetterBag]

    migrateGame :: ConnectionPool -> Entity M.Game -> IO ()
    migrateGame pool (Entity key (M.Game _ letterBag _ _ _ _ _ _ _)) = do
        when (isOldTileListFormat letterBag) $ do
            let newLetterBag = updateTileListFormat letterBag
            withPool pool $ update key [M.GameOriginalLetterBag =. newLetterBag]

    isOldTileListFormat :: T.Text -> Bool
    isOldTileListFormat = notElem ','
    withPool = flip runSqlPersistMPool

    updateTileListFormat :: T.Text -> T.Text
    updateTileListFormat = T.intersperse ','