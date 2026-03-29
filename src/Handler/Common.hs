{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import Yesod.Core (widgetToPageContent, PageContent(..), withUrlRenderer)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/svg+xml"
                        $ toContent $(embedFile "config/favicon.svg")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getServiceWorkerR :: Handler ()
getServiceWorkerR = do
    cacheSeconds 0
    addHeader "Service-Worker-Allowed" "/"
    sendFile "application/javascript" "static/sw.js"

getManifestR :: Handler ()
getManifestR = do
    cacheSeconds $ 60 * 60 * 24
    sendFile "application/manifest+json" "static/manifest.json"

defaultPageLayout :: Widget -> Handler Html
defaultPageLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>Wordify
                    <meta charset="UTF-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    <link rel="icon" type="image/svg+xml" href="@{FaviconR}">
                    <link rel="manifest" href="/manifest.json">
                    ^{pageHead pc}
                <body>
                    <div .special-wrapper>
                        ^{pageBody pc}
        |]
