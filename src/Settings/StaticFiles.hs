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

-- Cache-busted asset routes. The filenames below are updated by build-ui.sh
-- at build time to include a timestamp suffix, so browsers always fetch the
-- latest version instead of serving a stale cached copy.
--
-- The build splits the Vue UI into one JS bundle per page (round, create-game,
-- game-lobby, etc.), so each handler should reference only the JS bundle it
-- needs. CSS is shared across pages because almost all of it is global
-- (primeicons + tailwind + base styles), so a single CSS file is loaded by
-- every page and cached on the first hit.

wordifyCss :: StaticRoute
wordifyCss = StaticRoute ["css", "wordify-shared_20260505_174324.css"] []

wordifyRoundJs :: StaticRoute
wordifyRoundJs = StaticRoute ["js", "round_20260505_174324.js"] []

wordifyCreateGameJs :: StaticRoute
wordifyCreateGameJs = StaticRoute ["js", "create-game_20260505_174324.js"] []

wordifyGameLobbyJs :: StaticRoute
wordifyGameLobbyJs = StaticRoute ["js", "game-lobby_20260505_174324.js"] []

wordifyGameInviteJs :: StaticRoute
wordifyGameInviteJs = StaticRoute ["js", "game-invite_20260505_174324.js"] []

wordifyHomeJs :: StaticRoute
wordifyHomeJs = StaticRoute ["js", "home_20260505_174324.js"] []

wordifyLoginJs :: StaticRoute
wordifyLoginJs = StaticRoute ["js", "login_20260505_174324.js"] []

wordifyChooseUsernameJs :: StaticRoute
wordifyChooseUsernameJs = StaticRoute ["js", "choose-username_20260505_174324.js"] []
