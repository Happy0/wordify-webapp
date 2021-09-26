module OAuthDetails (OAuthDetails(OAuthDetails), buildOAuthDetails) where

    import Data.Either
    import qualified Data.Text as T
    import Data.Text.Encoding as TE
    import qualified Text.Show as TS
    import URI.ByteString (parseURI, URIRef, strictURIParserOptions, Absolute)

    data OAuthDetails = OAuthDetails {
        userDetailsEndpoint :: URIRef Absolute,
        authorizationEndpoint :: URIRef Absolute,
        oauthAccessTokenEndpoint :: URIRef Absolute,
        clientId :: T.Text,
        secret :: T.Text
    }

    buildOAuthDetails :: T.Text -> T.Text -> T.Text -> Either T.Text OAuthDetails
    buildOAuthDetails baseUrl oAuthClientId oauthSecret =
        do
            userProfile <- buildUri baseUrl "/userinfo"
            authorizeEndpoint <- buildUri baseUrl "/authorize"
            tokenEndpoint <- buildUri baseUrl "/oauth/token"
            Right OAuthDetails {
                userDetailsEndpoint = userProfile,
                authorizationEndpoint = authorizeEndpoint,
                oauthAccessTokenEndpoint = tokenEndpoint,
                clientId = oAuthClientId,
                secret = oauthSecret
            }


    buildUri :: T.Text -> T.Text -> Either T.Text (URIRef Absolute)
    buildUri baseUri suffix = 
        let result = parseURI strictURIParserOptions (TE.encodeUtf8 (T.concat [ baseUri, suffix] ))
        in case result of
            Left err ->  Left (T.pack (TS.show err))
            Right a -> Right a