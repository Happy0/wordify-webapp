module Controllers.User.Model.ServerUser(ServerUser(ServerUser), userId, username) where

    import Data.Text
    import Prelude

    data ServerUser = ServerUser {userId :: Text, username :: Maybe Text}
