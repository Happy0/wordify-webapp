{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings.StaticFiles where

import Settings     (appStaticDir, compileTimeAppSettings)
import Yesod.Static

-- This generates easy references to files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--
--     js_script_js
--
-- If the identifier is not available, you may use:
--
--     StaticFile ["js", "script.js"] []
staticFiles (appStaticDir compileTimeAppSettings)

-- Cache-busted routes. The filenames below are updated by build-ui.sh at build
-- time to include a timestamp suffix, so browsers always fetch the latest version
-- instead of serving a stale cached copy.
wordifyJs :: StaticRoute
wordifyJs = StaticRoute ["js", "wordify_20260301_143841.js"] []

wordifyCss :: StaticRoute
wordifyCss = StaticRoute ["css", "wordify_20260301_143841.css"] []
