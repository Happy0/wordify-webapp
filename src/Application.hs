{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Sqlite              (createSqlitePool, runSqlPool,
                                             sqlDatabase, sqlPoolSize)

import qualified Prelude as P

import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.HTTP.Client.TLS              (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home
import Handler.Game
import Handler.GameLobby
import Handler.AuthTest
import Data.List.Split
import qualified Data.Map as M
import Wordify.Rules.Dictionary
import Wordify.Rules.LetterBag
import Data.FileEmbed
import Control.Concurrent.Timer
import Control.Error.Util
import Control.Monad.Trans.Except
import Controllers.GameLobby.Model.GameLobby
import Control.Concurrent.Suspend
import Data.Time.Clock
import System.Environment
import System.Random
import Data.Char (isSpace)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    localisedGameSetups <- loadGameBundles
    gameLobbies <- newTVarIO M.empty
    games <- newTVarIO M.empty
    stdGen <- getStdGen
    randomGenerator <- newTVarIO stdGen

    authDetails <- getAuthDetails
    repeatedTimer (cleanIdleLobbies gameLobbies) (hDelay 48)

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePool
        (sqlDatabase $ appDatabaseConf appSettings)
        (sqlPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

getAuthDetails :: IO (Either Text AuthDetails)
getAuthDetails = runExceptT $
    do
        clientId <- ExceptT $ note "Missing AUTH_CLIENT_ID environment variable" <$> (lookupEnv "AUTH_CLIENT_ID")
        clientSecret <- ExceptT $ note "Missing AUTH_CLIENT_SECRET environment variable" <$> (lookupEnv "AUTH_CLIENT_SECRET")
        return $ AuthDetails (pack clientId) (pack clientSecret)


cleanIdleLobbies :: TVar (Map Text (TVar GameLobby)) -> IO ()
cleanIdleLobbies lobbies = do
    lobbiesMap <- readTVarIO lobbies
    let keyVals = M.assocs lobbiesMap
    timeNow <- getCurrentTime    

    flip mapM_ keyVals $
            \(key, val) ->
            do
                lobby <- readTVarIO val
                let opened = openedAt lobby
                let diff = diffUTCTime timeNow opened
                
                -- If the lobby hasn't resulted in a created game after an hour,
                -- remove it 
                when (diff >= (60 * 60)) $
                    atomically $
                        modifyTVar lobbies (M.delete key)
                

loadGameBundles :: IO LocalisedGameSetups
loadGameBundles =
    do
        localisations <- P.readFile "config/localisations.txt"
        gameBundles <- mapM loadGameBundle $ filter (not . all isSpace) $ splitOn ("\n" :: String) localisations
        return $ M.fromList gameBundles

loadGameBundle :: String -> IO (Text, LocalisedGameSetup)
loadGameBundle locale =
    do
        bag <- loadBag $ "config/localised_setups" ++ "/" ++ (locale) ++ "/" ++ "bag"
        dictionary <- loadDictionary $ "config/localised_setups" ++ "/" ++ (locale) ++ "/" ++ "dict"
        return $ (pack locale, GameSetup dictionary bag)

loadBag :: String -> IO LetterBag
loadBag locale =
    do
        bag <- makeBag locale
        case bag of
            -- Don't begin the app if we cannot load the config
            Left err -> error $ show err
            Right bg -> return bg

loadDictionary :: String -> IO Dictionary
loadDictionary locale =
    do
        dictionary <- makeDictionary locale
        case dictionary of
            -- Don't begin the app if we cannot load the config
            Left err -> error $ show err
            Right dict -> return dict


-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
