module Handler.Me where

    import Import
    import Data.Aeson ((.=), object)
    import qualified Controllers.User.UserController as UC
    import qualified Controllers.User.Model.ServerUser as SU

    getApiMeR :: Handler Value
    getApiMeR = do
        maybeUserId <- maybeAuthId
        case maybeUserId of
            Nothing -> returnJson $ object ["loggedIn" .= False]
            Just uid -> do
                app <- getYesod
                maybeUser <- liftIO $ UC.getUser (userController app) uid
                returnJson $ object
                    [ "loggedIn" .= True
                    , "username" .= (maybeUser >>= SU.username)
                    ]
