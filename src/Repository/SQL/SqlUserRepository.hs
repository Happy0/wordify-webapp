{-# LANGUAGE ScopedTypeVariables #-}
module Repository.SQL.SqlUserRepository (SqlUserRepositoryBackend (SqlUserRepositoryBackend)) where

import ClassyPrelude (Either (Left, Right), IO, Maybe (Just, Nothing), flip, return, ($), (.), (<), (<>))
import Control.Exception (SomeException, try)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Sql
import qualified Model as M
import Controllers.User.Model.ServerUser (ServerUser (ServerUser))
import Repository.UserRepository (UserRepository (createUserIfNotExists, getUser, setUsername, getUsernamesByPrefix), SetUsernameResult (..))

newtype SqlUserRepositoryBackend = SqlUserRepositoryBackend (Pool SqlBackend)

instance UserRepository SqlUserRepositoryBackend where
  createUserIfNotExists (SqlUserRepositoryBackend pool) = createUserIfNotExistsSQL pool
  getUser (SqlUserRepositoryBackend pool) = getUserSQL pool
  setUsername (SqlUserRepositoryBackend pool) = setUsernameSQL pool
  getUsernamesByPrefix (SqlUserRepositoryBackend pool) = getUsernamesByPrefixSQL pool

createUserIfNotExistsSQL :: Pool SqlBackend -> T.Text -> Maybe T.Text -> IO ()
createUserIfNotExistsSQL pool visitorId maybeNickname = do
  withPool pool $ do
    existing <- get (M.UserKey visitorId)
    case existing of
      Nothing -> insert_ (M.User visitorId maybeNickname Nothing)
      Just _ -> update (M.UserKey visitorId) [M.UserNickname =. maybeNickname]

getUserSQL :: Pool SqlBackend -> T.Text -> IO (Maybe ServerUser)
getUserSQL pool visitorId =
  withPool pool $ do
    userProfile <- selectFirst [M.UserIdent ==. visitorId] []
    case userProfile of
      Nothing -> return Nothing
      Just (Entity _ (M.User ident _ uname)) -> return $ Just (ServerUser ident uname)

setUsernameSQL :: Pool SqlBackend -> T.Text -> T.Text -> IO SetUsernameResult
setUsernameSQL pool visitorId uname = do
  result <- try $ withPool pool $ do
    maybeUser <- selectFirst [M.UserIdent ==. visitorId] []
    case maybeUser of
      Nothing -> return UserNotFound
      Just _ -> do
        update (M.UserKey visitorId) [M.UserUsername =. Just uname]
        return UsernameSet
  case result of
    Left (_ :: SomeException) -> return UsernameTaken
    Right outcome -> return outcome

getUsernamesByPrefixSQL :: Pool SqlBackend -> T.Text -> IO [T.Text]
getUsernamesByPrefixSQL pool prefix =
  if T.length prefix < 3
    then return []
    else withPool pool $ do
      results <- rawSql
        "SELECT username FROM \"user\" WHERE username LIKE ?"
        [PersistText (prefix <> "%")]
      return [u | Single (Just u) <- results]

withPool = flip runSqlPersistMPool
