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
    storeUser pool (AuthUser userId maybeNick) = do
        withPool pool $ do
            _ <- upsert (M.User userId maybeNick) []
            return ()


    getUser :: Pool SqlBackend -> Text -> IO (Maybe AuthUser)
    getUser pool userId =
        withPool pool $ do
            userProfile <- selectFirst [M.UserIdent ==. userId] []
            case userProfile of
                Nothing -> return Nothing
                Just (Entity _ (M.User ident nick)) -> return $ Just (AuthUser ident nick)
