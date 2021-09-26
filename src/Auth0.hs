module Auth0 (auth0Provider) where

    import Prelude
    import Yesod.Auth.OAuth2.Prelude
    import Controllers.User.Model.AuthUser(AuthUser(AuthUser))
    import qualified Data.Text as T
    import Data.Aeson
    import Debug.Trace
    import System.IO.Unsafe

    pluginName :: Text
    pluginName = "oauth0"

    -- TODO: Parameterise the domain and use environment variable to populate it
    auth0Provider :: YesodAuth m => Text -> Text -> AuthPlugin m
    auth0Provider clientId clientSecret =
        authOAuth2 pluginName oauth2 $ \manager token -> do
            (AuthUser userId nick, userResponse) <- authGetProfile
                pluginName
                manager
                token
                "https://dev--x0uvgaw.eu.auth0.com/userinfo"

            pure Creds
                { credsPlugin = pluginName
                , credsIdent = userId
                , credsExtra = setExtra token userResponse
                }
        where
            oauth2 = OAuth2
                { oauthClientId = clientId
                , oauthClientSecret = clientSecret
                , oauthOAuthorizeEndpoint = "https://dev--x0uvgaw.eu.auth0.com/authorize" `withQuery` [scopeParam " " ["auth", "read_user", "openid", "profile", "sub"]]
                , oauthAccessTokenEndpoint = "https://dev--x0uvgaw.eu.auth0.com/oauth/token"
                , oauthCallback = Nothing
                }