module Controllers.User.UserController
  ( UserController,
    makeUserController,
    createUserIfNotExists,
    getUser,
    setUsername,
    getUsernamesByPrefix,
  )
where

import ClassyPrelude (IO, Maybe)
import Controllers.User.Model.ServerUser (ServerUser)
import qualified Data.Text as T
import Repository.UserRepository (UserRepositoryImpl, SetUsernameResult, createUserIfNotExistsImpl, getUserImpl, setUsernameImpl, getUsernamesByPrefixImpl)

data UserController = UserController
  { userRepository :: UserRepositoryImpl
  }

makeUserController :: UserRepositoryImpl -> UserController
makeUserController = UserController

createUserIfNotExists :: UserController -> T.Text -> Maybe T.Text -> IO ()
createUserIfNotExists controller = createUserIfNotExistsImpl (userRepository controller)

getUser :: UserController -> T.Text -> IO (Maybe ServerUser)
getUser controller = getUserImpl (userRepository controller)

setUsername :: UserController -> T.Text -> T.Text -> IO SetUsernameResult
setUsername controller = setUsernameImpl (userRepository controller)

getUsernamesByPrefix :: UserController -> T.Text -> IO [T.Text]
getUsernamesByPrefix controller = getUsernamesByPrefixImpl (userRepository controller)
