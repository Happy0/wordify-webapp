{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( getApplicationDev,
    appMain,
    develMain,
    makeFoundation,
    makeLogWare,

    -- * for DevelMain
    getApplicationRepl,
    shutdownApp,

    -- * for GHCI
    handler,
    db,
  )
where

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Control.Concurrent
import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Control.Error.Util
import qualified Control.Monad as MO
import Control.Monad.Logger (liftLoc, runLoggingT)
import Control.Monad.Trans.Except
import Controllers.Chat.Chat (getChat)
import Controllers.Chat.Chatroom (Chatroom, freezeChatroom)
import Controllers.Common.CacheableSharedResource
import Controllers.Definition.DefinitionService (makeDefinitionService, anyDefinitionClient)
import Controllers.Definition.Clients.RaeApiClient (makeRaeApiClient)
import Controllers.Definition.Clients.FreeDictionaryClient (FreeDictionaryClient (FreeDictionaryClient))
import Controllers.Game.GameDefinitionController (makeGameDefinitionController)
import Modules.Notifications.Api (makeNotificationService)
import Web.WebPush (generateVAPIDKeys, readVAPIDKeys, vapidPublicKeyBytes, VAPIDKeys, VAPIDKeysMinDetails(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LB
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text.Encoding as TE
import Repository.PushNotificationRepository (toPushNotificationRepositoryImpl)
import Repository.SQL.SqlPushNotificationRepository (SqlPushNotificationRepositoryBackend (SqlPushNotificationRepositoryBackend))
import Repository.SQL.SqlNotificationRepository (SqlNotificationRepositoryBackend (SqlNotificationRepositoryBackend))
import Repository.UserRepository (toUserRepositoryImpl)
import Repository.SQL.SqlUserRepository (SqlUserRepositoryBackend (SqlUserRepositoryBackend))
import Repository.SQL.SqlLobbyRepository (SqlLobbyRepositoryBackend (SqlLobbyRepositoryBackend))
import Controllers.User.UserController (makeUserController, UserController)
import Controllers.Game.Model.ServerGame
import Controllers.Game.Persist (getGame, getLobby)
import Controllers.GameLobby.Model.GameLobby
import Data.Char (isSpace)
import Data.FileEmbed
import Data.List.Split
import qualified Data.Map as M
import Data.Time.Clock
import Database.Persist
import Data.Pool (withResource)
import Control.Lens ((&), (.~))
import Database.Persist.Sqlite
  ( createSqlitePoolFromInfo,
    extraPragmas,
    mkSqliteConnectionInfo,
    runSqlPool,
    sqlDatabase,
    sqlPoolSize,
  )

import Handler.AuthTest
import Handler.Common
import Handler.Game
import Handler.GameLobby
import Handler.Home
import Handler.Login
import Handler.Me
import Handler.Notifications
import Handler.Push
import Handler.Username
import Import
import InactivityTracker
import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    defaultShouldDisplayException,
    getPort,
    runSettings,
    setHost,
    setOnException,
    setPort,
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (Logger),
    IPAddrSource (..),
    OutputFormat (..),
    destination,
    mkRequestLogger,
    outputFormat,
  )
import OAuthDetails (OAuthDetails, buildOAuthDetails)
import Repository.ChatRepository
import Repository.DefinitionRepository (toDefinitionRepositoryImpl)
import Repository.SQL.SqlChatRepository
import Repository.SQL.SqlDefinitionRepository (DefinitionRepositorySQLBackend (DefinitionRepositorySQLBackend))
import Repository.SQL.Migrations (runMigrations)
import Repository.SQL.Setup (runSetup)
import System.Environment
import System.Log.FastLogger
  ( defaultBufSize,
    newStdoutLoggerSet,
    toLogStr,
  )
import System.Random
import Wordify.Rules.Dictionary
import Wordify.Rules.LetterBag
import Wordify.Rules.Extra.SpanishExtraRule (spanishGameExtraRules)
import qualified Prelude as P
import Model.GameSetup (LocalisedGameSetup (GameSetup), TileValues)
import Controllers.Definition.Clients.WiktionaryClient (WiktionaryClient, makeWiktionaryClient)
import Wordify.Rules.Tile (tileValue)
import qualified Data.Text as T
import Modules.UserEvent.Api (makeUserEventService)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> TVar InactivityTracker -> IO App
makeFoundation appSettings inactivityTracker = do
  -- Some basic initializations: HTTP connection manager, logger, and static
  -- subsite.
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
      (appStaticDir appSettings)

  localisedGameSetups <- loadGameBundles
  stdGen <- getStdGen
  randomGenerator <- newTVarIO stdGen

  authDetails <- getAuthDetails

  maybeVapidKeysDir <- lookupEnv "VAPID_KEYS_DIR"
  maybeVapidKeys <- case maybeVapidKeysDir of
    Nothing -> return Nothing
    Just dir -> fmap Just (loadOrGenerateVAPIDKeys dir)
  let vapidPublicKey = fmap vapidPublicKeyToBase64 maybeVapidKeys

  -- The App {..} syntax is an example of record wild cards. For more
  -- information, see:
  -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
  let mkFoundation appConnPool games gameLobbies gameDefinitionController chatRooms userEventService notificationService userController lobbyRepository = App {..}

  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let tempFoundation = mkFoundation (error "connPool forced in tempFoundation") (error "game cache forced in tempFoundation") (error "game lobby cache forced in tempFoundation") (error "Definition repository forced in tempFoundation") (error "Chatroom cache forced in tempFoundation") (error "Game user event cache forced in tempFoundation") (error "Notification service forced in tempFoundation") (error "User controller forced in tempFoundation") (error "Lobby repository forced in tempFoundation")
  let logFunc = messageLoggerSource tempFoundation appLogger

  -- Create the database connection pool, setting busy_timeout on every
  -- connection so that concurrent writers retry instead of failing immediately.
  pool <-
    flip runLoggingT logFunc
      $ createSqlitePoolFromInfo
        ( mkSqliteConnectionInfo (sqlDatabase $ appDatabaseConf appSettings)
            & extraPragmas .~ ["PRAGMA busy_timeout = 5000", "PRAGMA foreign_keys = ON"]
        )
        (sqlPoolSize $ appDatabaseConf appSettings)

  -- Run manual migrations before Persistent's migrateAll. These handle
  -- schema changes (e.g. primary key restructuring) that migrateAll cannot
  -- express, and must see a consistent database state to copy data safely.
  runLoggingT (runSqlPool runMigrations pool) logFunc

  -- Perform automatic schema migration.
  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

  -- Run additional database setup (indexes, etc.)
  runLoggingT (runSqlPool runSetup pool) logFunc

  let chatRepository = toChatRepositoryImpl (SqlChatRepositoryBackend pool)
  chatrooms <- makeGlobalResourceCache (getChat chatRepository) (Just freezeChatroom)

  let userRepo = toUserRepositoryImpl (SqlUserRepositoryBackend pool)
  let userCtrl = makeUserController userRepo

  games <- makeGlobalResourceCache (getGame pool userCtrl localisedGameSetups) Nothing
  gameLobbies <- makeGlobalResourceCache (getLobby pool userCtrl localisedGameSetups) Nothing

  userEventService <- makeUserEventService

  let definitionService = makeDefinitionService [anyDefinitionClient makeWiktionaryClient, anyDefinitionClient (makeRaeApiClient Nothing)]
  let definitionRepository = toDefinitionRepositoryImpl (DefinitionRepositorySQLBackend pool)
  gameDefinitionController <- makeGameDefinitionController definitionService definitionRepository

  let pushNotificationRepository = toPushNotificationRepositoryImpl (SqlPushNotificationRepositoryBackend pool)
  let notificationRepository = SqlNotificationRepositoryBackend pool
  notifSvc <- makeNotificationService notificationRepository pushNotificationRepository maybeVapidKeys appHttpManager userEventService

  let lobbyRepo = SqlLobbyRepositoryBackend pool

  -- Return the foundation
  return $ mkFoundation pool games gameLobbies gameDefinitionController chatrooms userEventService notifSvc userCtrl lobbyRepo

getAuthDetails :: IO (Either Text OAuthDetails)
getAuthDetails =
  do
    runExceptT
      $ do
        clientId <- ExceptT $ note "Missing AUTH_CLIENT_ID environment variable" <$> lookupEnv "AUTH_CLIENT_ID"
        clientSecret <- ExceptT $ note "Missing AUTH_CLIENT_SECRET environment variable" <$> lookupEnv "AUTH_CLIENT_SECRET"
        authBaseUri <- ExceptT $ note "Missing AUTH_BASE_URI environment variable" <$> lookupEnv "AUTH_BASE_URI"
        except $ buildOAuthDetails (pack authBaseUri) (pack clientId) (pack clientSecret)

loadOrGenerateVAPIDKeys :: FilePath -> IO VAPIDKeys
loadOrGenerateVAPIDKeys dir = do
  let keyFile = dir </> "vapid_keys.json"
  exists <- doesFileExist keyFile
  if exists
    then do
      contents <- LB.readFile keyFile
      case decodeVAPIDKeysMinDetails contents of
        Just details -> return (readVAPIDKeys details)
        Nothing -> error "Failed to parse VAPID keys from vapid_keys.json"
    else do
      createDirectoryIfMissing True dir
      details <- generateVAPIDKeys
      LB.writeFile keyFile (encodeVAPIDKeysMinDetails details)
      return (readVAPIDKeys details)

vapidPublicKeyToBase64 :: VAPIDKeys -> Text
vapidPublicKeyToBase64 keys =
  let b64 = B64URL.encode (BS.pack (vapidPublicKeyBytes keys))
  in TE.decodeUtf8 (fst (BS.breakSubstring "=" b64))

encodeVAPIDKeysMinDetails :: VAPIDKeysMinDetails -> LB.ByteString
encodeVAPIDKeysMinDetails (VAPIDKeysMinDetails priv pubX pubY) =
  A.encode $ A.object ["privateNumber" A..= priv, "publicCoordX" A..= pubX, "publicCoordY" A..= pubY]

decodeVAPIDKeysMinDetails :: LB.ByteString -> Maybe VAPIDKeysMinDetails
decodeVAPIDKeysMinDetails bs = do
  obj <- A.decode bs
  flip A.parseMaybe obj $ A.withObject "VAPIDKeysMinDetails" $ \o ->
    VAPIDKeysMinDetails <$> o A..: "privateNumber" <*> o A..: "publicCoordX" <*> o A..: "publicCoordY"

getExitAppOnIdleConfig :: IO Bool
getExitAppOnIdleConfig = do
  exitOnIdle <- lookupEnv "EXIT_ON_IDLE"
  case exitOnIdle of
    Nothing -> pure False
    Just configValue -> pure (toLower configValue == "true")

loadGameBundles :: IO LocalisedGameSetups
loadGameBundles =
  do
    localisations <- P.readFile "config/localisations.txt"
    gameBundles <- mapM loadGameBundle $ filter (not . all isSpace) $ splitOn ("\n" :: String) localisations
    return $ M.fromList gameBundles

getTileValues :: ValidTiles -> TileValues
getTileValues letters =
  let values = M.map tileValue letters
  in let valuesWithText = M.mapKeys T.pack values
  in valuesWithText

loadGameBundle :: String -> IO (Text, LocalisedGameSetup)
loadGameBundle locale =
  do
    bag <- loadBag $ "config/localised_setups" ++ "/" ++ locale ++ "/" ++ "bag"
    dictionary <- loadDictionary $ "config/localised_setups" ++ "/" ++ locale ++ "/" ++ "dict"

    let tileValues = getTileValues (validLetters bag)

    case locale of
      "es_fise" -> return (pack locale, GameSetup "es" dictionary bag spanishGameExtraRules tileValues)
      _ -> return (pack locale, GameSetup (pack locale) dictionary bag [] tileValues)


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
  mkRequestLogger
    def
      { outputFormat =
          if appDetailedRequestLogging $ appSettings foundation
            then Detailed True
            else
              Apache
                ( if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket
                ),
        destination = Logger $ loggerSet $ appLogger foundation
      }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException
      ( \_req e ->
          when (defaultShouldDisplayException e)
            $ messageLoggerSource
              foundation
              (appLogger foundation)
              $(qLocation >>= liftLoc)
              "yesod"
              LevelError
              (toLogStr $ "Exception from Warp: " ++ show e)
      )
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  inactivityTracker <- makeInactivityTracker
  settings <- getAppSettings
  foundation <- makeFoundation settings inactivityTracker
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
  settings <-
    loadYamlSettingsArgs
      -- fall back to compile-time values, set to [] to require values at runtime
      [configSettingsYmlValue]
      -- allow environment variables to override
      useEnv

  inactivityTracker <- makeInactivityTracker

  -- Generate the foundation from the settings
  foundation <- makeFoundation settings inactivityTracker

  -- Generate a WAI Application from the foundation
  app <- makeApplication foundation

  exitAppOnIdle <- getExitAppOnIdleConfig

  let runApp = runSettings (warpSettings foundation) app

  (if exitAppOnIdle then raceUntilInactive inactivityTracker runApp else runApp)

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  inactivityTracker <- makeInactivityTracker
  settings <- getAppSettings
  foundation <- makeFoundation settings inactivityTracker
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
handler h = do
  inactivityTracker <- makeInactivityTracker
  getAppSettings >>= (`makeFoundation` inactivityTracker) >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
