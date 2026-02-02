{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Login where

import Import

-- | Handle login requests by storing the return URL in the session
-- before redirecting to the OAuth provider.
-- The "_ULT" key is Yesod Auth's "Ultimate Destination" - after login,
-- the user will be redirected there instead of the default loginDest.
getLoginR' :: Handler Html
getLoginR' = do
    -- Try to get return URL from query parameter first
    maybeReturnUrl <- lookupGetParam "returnUrl"

    -- Store the return URL in the session if provided
    case maybeReturnUrl of
        Just returnUrl -> setSession "_ULT" returnUrl
        Nothing -> return ()

    -- Redirect to the OAuth2 forward endpoint
    redirect ("/auth/page/oauth0/forward" :: Text)
