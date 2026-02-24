module Repository.UserRepository
  ( toUserRepositoryImpl,
    UserRepository (createUserIfNotExists, getUser, setUsername, getUsernamesByPrefix),
    UserRepositoryImpl,
    createUserIfNotExistsImpl,
    getUserImpl,
    setUsernameImpl,
    getUsernamesByPrefixImpl,
    SetUsernameResult (..),
  )
where

import ClassyPrelude (IO, Maybe)
import Controllers.User.Model.ServerUser (ServerUser)
import qualified Data.Text as T

data SetUsernameResult = UsernameSet | UsernameTaken | UserNotFound

class UserRepository a where
  createUserIfNotExists :: a -> T.Text -> Maybe T.Text -> IO ()
  getUser :: a -> T.Text -> IO (Maybe ServerUser)
  setUsername :: a -> T.Text -> T.Text -> IO SetUsernameResult
  getUsernamesByPrefix :: a -> T.Text -> IO [T.Text]

data UserRepositoryImpl = UserRepositoryImpl
  { createUserIfNotExistsField :: T.Text -> Maybe T.Text -> IO (),
    getUserField :: T.Text -> IO (Maybe ServerUser),
    setUsernameField :: T.Text -> T.Text -> IO SetUsernameResult,
    getUsernamesByPrefixField :: T.Text -> IO [T.Text]
  }

toUserRepositoryImpl :: (UserRepository a) => a -> UserRepositoryImpl
toUserRepositoryImpl repository =
  UserRepositoryImpl
    { createUserIfNotExistsField = createUserIfNotExists repository,
      getUserField = getUser repository,
      setUsernameField = setUsername repository,
      getUsernamesByPrefixField = getUsernamesByPrefix repository
    }

createUserIfNotExistsImpl :: UserRepositoryImpl -> T.Text -> Maybe T.Text -> IO ()
createUserIfNotExistsImpl = createUserIfNotExistsField

getUserImpl :: UserRepositoryImpl -> T.Text -> IO (Maybe ServerUser)
getUserImpl = getUserField

setUsernameImpl :: UserRepositoryImpl -> T.Text -> T.Text -> IO SetUsernameResult
setUsernameImpl = setUsernameField

getUsernamesByPrefixImpl :: UserRepositoryImpl -> T.Text -> IO [T.Text]
getUsernamesByPrefixImpl = getUsernamesByPrefixField
