module Handler.Push where
    import ClassyPrelude (Maybe(..), pure, ($), liftIO, (<>), lookup)
    import Foundation (Handler, App (pushController))
    import Data.Aeson (FromJSON, (.:))
    import Data.Aeson.Types (FromJSON(parseJSON))
    import Data.Aeson (withObject)
    import ClassyPrelude.Yesod (requireCheckJsonBody, notAuthenticated, getYesod, guessApproot)
    import Import (YesodAuth (maybeAuthId))
    import Controllers.Push.PushController (PushTokenSubscription(..), subscribe)
    import Yesod.Core (waiRequest)
    import Network.Wai (requestHeaders, isSecure)
    import Data.Text.Encoding (decodeUtf8)
    import qualified Data.Text as T

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
        req <- waiRequest

        let headers = requestHeaders req
            hostHeader = case lookup "Host" headers of
                Just h  -> decodeUtf8 h
                Nothing -> "127.0.0.1:4000"
            scheme = case lookup "X-Forwarded-Proto" headers of
                Just "https" -> "https"
                Just _       -> "http"
                Nothing      -> if isSecure req then "https" else "http"
            baseUrl = scheme <> "://" <> hostHeader

        case userId of
            Nothing -> notAuthenticated
            Just uid -> liftIO $ subscribe (pushController app) uid baseUrl subscription
