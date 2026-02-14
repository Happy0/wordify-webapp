module Handler.Username where

    import Import
    import Import.NoFoundation (wordifyCss, wordifyJs)
    import Data.Aeson (FromJSON, (.:), (.=), object, withObject)
    import Controllers.User.UserController (setUsername)
    import Repository.UserRepository (SetUsernameResult(..))
    import Yesod.Core (sendStatusJSON)
    import Network.HTTP.Types.Status (status400, status409)
    import Handler.Home (gamePagelayout)
    import qualified Data.Text as T
    import Data.Char (isAlphaNum)

    getChooseUsernameR :: Handler Html
    getChooseUsernameR = do
        _ <- requireAuthId
        gamePagelayout $ do
            addStylesheet $ StaticR wordifyCss
            addScript $ StaticR wordifyJs
            [whamlet|
                <div #choose-username-container>
            |]
            toWidget
                [julius|
                    Wordify.createChooseUsername('#choose-username-container', {
                        redirectUrl: '/'
                    });
                |]

    newtype UsernameRequest = UsernameRequest { username :: Text }

    instance FromJSON UsernameRequest where
        parseJSON = withObject "UsernameRequest" $ \obj -> do
            uname <- obj .: "username"
            pure (UsernameRequest uname)

    postSetUsernameR :: Handler ()
    postSetUsernameR = do
        req <- requireCheckJsonBody :: Handler UsernameRequest
        uid <- requireAuthId
        app <- getYesod

        case validateUsername (username req) of
            Just err -> sendStatusJSON status400 (object ["error" .= ("invalid_username" :: Text), "message" .= err])
            Nothing -> do
                result <- liftIO $ setUsername (userController app) uid (username req)
                case result of
                    UsernameSet -> return ()
                    UsernameTaken -> sendStatusJSON status409 (object ["error" .= ("username_taken" :: Text), "message" .= ("This username is already taken. Please choose another." :: Text)])
                    UserNotFound -> notAuthenticated

    validateUsername :: Text -> Maybe Text
    validateUsername uname
        | T.length uname < 3 = Just "Username must be at least 3 characters."
        | T.length uname > 15 = Just "Username must be 15 characters or fewer."
        | not (T.all isValidUsernameChar uname) = Just "Username may only contain letters, numbers, hyphens, and underscores."
        | otherwise = Nothing
      where
        isValidUsernameChar c = isAlphaNum c || c == '-' || c == '_'
