module Handler.Username where

    import Import
    import Import.NoFoundation (wordifyCss, wordifyJs)
    import Data.Aeson (FromJSON, (.:), (.=), object, withObject)
    import Controllers.User.UserController (setUsername)
    import Repository.UserRepository (SetUsernameResult(..))
    import Yesod.Core (sendStatusJSON)
    import Network.HTTP.Types.Status (status409)
    import Handler.Home (gamePagelayout)

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

        result <- liftIO $ setUsername (userController app) uid (username req)
        case result of
            UsernameSet -> return ()
            UsernameTaken -> sendStatusJSON status409 (object ["error" .= ("username_taken" :: Text), "message" .= ("This username is already taken. Please choose another." :: Text)])
            UserNotFound -> notAuthenticated
