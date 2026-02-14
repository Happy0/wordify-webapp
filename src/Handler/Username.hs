module Handler.Username where

    import Import
    import Import.NoFoundation (css_wordify_css, wordifyJs)
    import Data.Aeson (FromJSON, (.:), (.=), object, withObject)
    import Controllers.User.UserController (setUsername)
    import Repository.UserRepository (SetUsernameResult(..))
    import Yesod.Core (sendStatusJSON)
    import Network.HTTP.Types.Status (status409)
    import Handler.Home (gamePagelayout)

    getChooseUsernameR :: Handler Html
    getChooseUsernameR = do
        maybeUserId <- maybeAuthId
        case maybeUserId of
            Nothing -> notAuthenticated
            Just _ ->
                gamePagelayout $ do
                    addStylesheet $ StaticR css_wordify_css
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
        userId <- maybeAuthId
        app <- getYesod

        case userId of
            Nothing -> notAuthenticated
            Just uid -> do
                result <- liftIO $ setUsername (userController app) uid (username req)
                case result of
                    UsernameSet -> return ()
                    UsernameTaken -> sendStatusJSON status409 (object ["error" .= ("username_taken" :: Text), "message" .= ("This username is already taken. Please choose another." :: Text)])
                    UserNotFound -> notAuthenticated
