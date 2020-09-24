module Controllers.User.Persist(storeUser, getUser) where

    import Prelude
    import Controllers.User.Model.AuthUser
    import Data.Pool
    import Database.Persist.Sql
    import System.IO
    import Data.Maybe
    import Data.Text
    import qualified Model as M

    withPool pool = flip runSqlPersistMPool pool
    
    storeUser :: Pool SqlBackend -> AuthUser -> IO ()
    storeUser pool (AuthUser userId maybeName maybeNick) = do
        withPool pool $ do
            _ <- upsert (M.User userId maybeName maybeNick) []
            return ()


    getUser :: Pool SqlBackend -> Text -> IO (Maybe AuthUser)
    getUser pool userId =
        withPool pool $ do
            userProfile <- selectFirst [M.UserIdent ==. userId] []
            case userProfile of
                Nothing -> return Nothing
                Just (Entity _ (M.User ident name nick)) -> return $ Just (AuthUser ident name nick)
