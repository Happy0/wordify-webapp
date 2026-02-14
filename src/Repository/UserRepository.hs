module Repository.UserRepository
  ( toUserRepositoryImpl,
    UserRepository (createUserIfNotExists, getUser, setUsername),
    UserRepositoryImpl,
    createUserIfNotExistsImpl,
    getUserImpl,
    setUsernameImpl,
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

data UserRepositoryImpl = UserRepositoryImpl
  { createUserIfNotExistsField :: T.Text -> Maybe T.Text -> IO (),
    getUserField :: T.Text -> IO (Maybe ServerUser),
    setUsernameField :: T.Text -> T.Text -> IO SetUsernameResult
  }

toUserRepositoryImpl :: (UserRepository a) => a -> UserRepositoryImpl
toUserRepositoryImpl repository =
  UserRepositoryImpl
    { createUserIfNotExistsField = createUserIfNotExists repository,
      getUserField = getUser repository,
      setUsernameField = setUsername repository
    }

createUserIfNotExistsImpl :: UserRepositoryImpl -> T.Text -> Maybe T.Text -> IO ()
createUserIfNotExistsImpl = createUserIfNotExistsField

getUserImpl :: UserRepositoryImpl -> T.Text -> IO (Maybe ServerUser)
getUserImpl = getUserField

setUsernameImpl :: UserRepositoryImpl -> T.Text -> T.Text -> IO SetUsernameResult
setUsernameImpl = setUsernameField
