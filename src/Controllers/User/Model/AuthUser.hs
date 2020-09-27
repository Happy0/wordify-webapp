module Controllers.User.Model.AuthUser(AuthUser(AuthUser), ident, nickname) where

    import Data.Aeson
    import Data.Text
    import Prelude

    instance FromJSON AuthUser where
        parseJSON (Object v) =
            do
                sub <- v .: "sub"
                nick <- v .:? "nickname"
                return (AuthUser sub nick)
        parseJSON _ = error "Unexpected format for JSON profile"

    data AuthUser = AuthUser {ident:: Text, nickname :: Maybe Text} deriving Show
