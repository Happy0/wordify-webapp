module Auth0 (auth0Provider) where

import Controllers.User.Model.AuthUser (AuthUser (AuthUser))
import Data.Aeson
import qualified Data.Text as T
import Debug.Trace
import OAuthDetails
import System.IO.Unsafe
import Yesod.Auth.OAuth2.Prelude
import Prelude

pluginName :: Text
pluginName = "oauth0"

-- TODO: Parameterise the domain and use environment variable to populate it
auth0Provider :: YesodAuth m => OAuthDetails -> AuthPlugin m
auth0Provider (OAuthDetails userDetailsEndpoint oauthAuthorizationEndpoint oauthAccessTokenEndpoint clientId clientSecret) =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (AuthUser userId nick, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        userDetailsEndpoint

    pure
      Creds
        { credsPlugin = pluginName,
          credsIdent = userId,
          credsExtra = setExtra token userResponse
        }
  where
    oauth2 =
      OAuth2
        { oauth2ClientId = clientId,
          oauth2ClientSecret = Just clientSecret,
          oauth2AuthorizeEndpoint = oauthAuthorizationEndpoint `withQuery` [scopeParam " " ["auth", "read_user", "openid", "profile", "sub"]],
          oauth2TokenEndpoint = oauthAccessTokenEndpoint,
          oauth2RedirectUri = Nothing
        }