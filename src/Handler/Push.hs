module Handler.Push where
    import ClassyPrelude (Maybe(..), pure, ($), liftIO)
    import Foundation (Handler, App (pushController))
    import Data.Aeson (FromJSON, (.:))
    import Data.Aeson.Types (FromJSON(parseJSON))
    import Data.Aeson (withObject)
    import ClassyPrelude.Yesod (requireCheckJsonBody, notAuthenticated, getYesod)
    import Import (YesodAuth (maybeAuthId))
    import Controllers.Push.PushController (PushTokenSubscription(..), subscribe)

    instance FromJSON PushTokenSubscription where
        parseJSON = withObject "PushTokenSubscription" $ \obj -> do
            auth <- obj .: "auth"
            p256dh <- obj .: "p256dh"
            endpoint <- obj .: "endpoint"
            expirationTime <- obj .: "expirationTime"
            pure (PushTokenSubscription auth p256dh endpoint expirationTime)

    postSubscribeR :: Handler ()
    postSubscribeR = do
        subscription <- requireCheckJsonBody :: Handler PushTokenSubscription
        userId <- maybeAuthId
        app <- getYesod

        case userId of
            Nothing -> notAuthenticated
            Just uid -> liftIO $ subscribe (pushController app) uid subscription
