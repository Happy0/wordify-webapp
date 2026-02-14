module Handler.AuthTest where

    import Prelude
    import Import(Handler,       Show(show))
    import Foundation
    import Yesod.Core ( whamlet, Html, Yesod(defaultLayout) )
    import Yesod.Auth
    import Yesod.Auth.OAuth2(getUserResponse)
    import Yesod
    import           Data.Default                (def)
    import           Data.Text                   (Text)
    import Controllers.User.UserController(getUser)

    getLoginTestPageR :: Handler Html
    getLoginTestPageR = do
        app <- getYesod
        let pool = appConnPool app
        maid <- maybeAuthId
        authId <- maybeAuthId

        profileDetails <- case authId of
            Just ident -> liftIO $ getUser (userController app) ident
            Nothing -> return Nothing

        defaultLayout
            [whamlet|
                $maybe _ <- maid
                    <p>
                        <a href=@{AuthR LogoutR}>Logout
                $nothing
                    <p>
                        <a href=@{AuthR LoginR}>Go to the login page
            |]
            