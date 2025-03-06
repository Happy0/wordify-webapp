{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

-- Used only when in "auth-dummy-login" setting is enabled.

import Auth0 (auth0Provider)
import Control.Monad.Logger (LogSource)
import Controllers.Common.CacheableSharedResource
import Controllers.Game.Model.ServerGame
import Controllers.GameLobby.Model.GameLobby
import Controllers.User.Model.AuthUser (AuthUser)
import Controllers.User.Persist (storeUser)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import InactivityTracker
import OAuthDetails (OAuthDetails, buildOAuthDetails)
import System.Environment
import System.Random
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Wordify.Rules.Dictionary
import Wordify.Rules.LetterBag
import Yesod.Auth.Dummy
import Yesod.Auth.OAuth2
import Yesod.Auth.OpenId (IdentifierType (Claimed), authOpenId)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

data LocalisedGameSetup = GameSetup
  { localisedDictionary :: Dictionary,
    localisedLetterBag :: LetterBag
  }

data AuthDetails = AuthDetails
  { clientId :: Text,
    clientSecret :: Text
  }

instance Show AuthDetails where
  show (AuthDetails clientId clientSecret) = "ClientId: " ++ (unpack clientId) ++ " ClientSecret: *****"

type LocalisedGameSetups = Map Text LocalisedGameSetup

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings :: AppSettings,
    -- | Settings for static file serving.
    appStatic :: Static,
    -- | Database connection pool.
    appConnPool :: ConnectionPool,
    appHttpManager :: Manager,
    appLogger :: Logger,
    localisedGameSetups :: LocalisedGameSetups,
    -- TODO: https://hackage.haskell.org/package/stm-containers
    gameLobbies :: TVar (Map Text (TVar GameLobby)),
    games :: ResourceCache Text ServerGame,
    randomGenerator :: TVar StdGen,
    authDetails :: Either Text OAuthDetails,
    inactivityTracker :: TVar InactivityTracker
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text,
    menuItemRoute :: Route App,
    menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

appHeader :: WidgetFor App ()
appHeader =
  do
    maid <- maybeAuthId
    toWidget
      [whamlet|
        <div style="padding: 5px 0px 15px 10px;border-bottom: 1px solid #e0e0e0;height: 30px;width: 100%;background: white;z-index: 1000;">
          <span>
            <a href=@{HomeR}> Home |
          ^{createGameButton}
          <span>
            <a href="http://www.github.com/happy0/wordify-webapp"> Source Code
          ^{loginLogout}
        |]
    toWidget
      [julius|
          var gameGameLinkClicked = function() {
              $("#create-game-lobby").modal();
          };
        |]
    $(widgetFile "game-dialog")
  where
    numPlayerOptions = [2 .. 4]

-- For now just don't show 'create game' button if the user isn't logged in.
-- Todo: Prompt the user to log in
createGameButton :: Widget
createGameButton = do
  maid <- maybeAuthId
  [whamlet|
    $maybe _ <- maid
        <span>
            <a href="javascript:gameGameLinkClicked()"> Create Game |
    $nothing
        <span>
    |]

loginLogout :: Widget
loginLogout =
  do
    maid <- maybeAuthId

    toWidget
      [hamlet|
            $maybe _ <- maid
                <span style="float: right; margin-right: 10px;">
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <span style="float: right; margin-right: 10px;">
                    <a href=@{AuthR LoginR}>Login
        |]

-- | A convenient synonym for database access functions.
type DB a =
  forall (m :: * -> *).
  (MonadIO m) =>
  ReaderT SqlBackend m a

getSessionBackendCertificateDirectory :: IO String
getSessionBackendCertificateDirectory = do
  configuredValue <- lookupEnv "SESSION_BACKEND_CERTIFICATE_DIRECTORY"
  return $ fromMaybe "config" configuredValue

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ = do
    certificateDirectory <- getSessionBackendCertificateDirectory
    Just <$> defaultClientSessionBackend 4320 (certificateDirectory ++ "/client_session_key.aes")

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    mcurrentRoute <- getCurrentRoute

    pc <- widgetToPageContent $ do
      addScriptRemote "//code.jquery.com/jquery-1.10.2.js"
      addScriptRemote "//code.jquery.com/ui/1.11.4/jquery-ui.js"
      addScriptRemote "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
      addStylesheetRemote "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
      addStylesheet $ StaticR css_bootstrap_css
      addStylesheet $ StaticR css_common_css
      appHeader
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute ::
    App ->
    Maybe (Route App)
  authRoute _ = Just $ AuthR LoginR

  isAuthorized ::
    -- | The route the user is visiting.
    Route App ->
    -- | Whether or not this is a "write" request.
    Bool ->
    Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  -- Default to Authorized for now.
  isAuthorized _ _ = return Authorized

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    -- | The file extension
    Text ->
    -- | The MIME content type
    Text ->
    -- | The contents of the file
    LByteString ->
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = Text
  authenticate creds = do
    let userProfile :: Either String AuthUser
        userProfile = getUserResponseJSON creds

    app <- getYesod

    case userProfile of
      Left _ -> return ()
      Right profile -> liftIO $ do
        storeUser (appConnPool app) profile

    return . Authenticated . credsIdent $ creds

  onLogin :: (MonadHandler m, master ~ HandlerSite m) => m ()
  onLogin = return ()

  redirectToReferer _ = True

  loginDest _ = HomeR
  logoutDest _ = HomeR

  authPlugins app = case authDetails app of
    Right oAuthDetails -> [auth0Provider oAuthDetails]
    Left err -> []

  -- The default maybeAuthId assumes a Persistent database. We're going for a
  -- simpler AuthId, so we'll just do a direct lookup in the session.
  maybeAuthId = lookupSession "_ID"

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _ -> Authorized

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
