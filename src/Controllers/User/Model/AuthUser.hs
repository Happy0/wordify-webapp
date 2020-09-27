module Controllers.User.Model.AuthUser(AuthUser(AuthUser), ident, name, nickname) where

    import Data.Aeson
    import Data.Text
    import Prelude

    instance FromJSON AuthUser where
        parseJSON (Object v) =
            do
                sub <- v .: "sub"
                name <- v .:? "name"
                nick <- v .:? "nickname"
                return (AuthUser sub name nick)
        parseJSON _ = error "Unexpected format for JSON profile"

    data AuthUser = AuthUser {ident:: Text, name:: Maybe Text, nickname :: Maybe Text} deriving Show
