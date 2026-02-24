{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Handler.GameLobby where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Loops
import Controllers.Common.CacheableSharedResource (getCacheableResource, withCacheableResource)
import Controllers.Game.Api
import Controllers.Game.Model.ServerGame
import Controllers.Game.Model.ServerPlayer
import Controllers.GameLobby.Api
import Controllers.GameLobby.CreateGame
import Controllers.GameLobby.GameLobby
import Controllers.GameLobby.Model.GameLobby
import qualified Controllers.GameLobby.Model.GameLobby as GL
import Controllers.User.Model.AuthUser (AuthUser (AuthUser))
import Repository.LobbyRepository (InvitePlayerResult (..))
import Data.Aeson
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Foundation
import Import
import InactivityTracker
import Model.Api
import Network.Mail.Mime
import qualified Network.WebSockets.Connection as C
import System.Random
import Web.Cookie
import Yesod.Core
import Yesod.WebSockets
import Network.HTTP.Types.Status (status404, status422)

getCreateGamePageR :: Handler Html
getCreateGamePageR = do
    app <- getYesod
    liftIO $ trackRequestReceivedActivity (inactivityTracker app)
    maid <- maybeAuthId
    case maid of
      Nothing -> gamePagelayout $ renderNotLoggedInLobbyPage "Login / Sign Up to Join the Lobby"
      Just _ -> do
        _ <- requireUsername
        gamePagelayout $ do
          addStylesheet $ (StaticR wordifyCss)
          addScript $ StaticR wordifyJs
          [whamlet|
            <div #createlobby>

              |]
          toWidget
            [julius|

              const lobby = Wordify.createCreateGame('#createlobby', {
                locales: {
                  "English": "en",
                  "Spanish (FISE)": "es_fise"
                },
                isLoggedIn: true
              });
            |]

{-
  Creates a new game lobby for inviting players to a game via a link.

  Returns the gameId of the new game lobby for the client to use to join
  the new lobby.
-}
postCreateGameR :: Handler Text
postCreateGameR =
  do
    app <- getYesod
    liftIO $ trackRequestReceivedActivity (inactivityTracker app)
    CreateGameLobby numPlayers locale <- requireJsonBody :: Handler CreateGameLobby
    gameCreateResult <- liftIO (createGame app numPlayers locale)
    case gameCreateResult of
      Left err -> invalidArgs [err]
      Right gameId -> return gameId

renderNotLoggedInLobbyPage :: Text -> WidgetFor App ()
renderNotLoggedInLobbyPage message = do
  addStylesheet $ (StaticR wordifyCss)
  addScript $ StaticR wordifyJs
  [whamlet|
    <div #lobbylogin>
        
      |]
  toWidget
    [julius|
      var url = document.URL;
      var webSocketUrl = url.replace("http:", "ws:").replace("https:", "wss:");
      
      const login = Wordify.createLogin('#lobbylogin', {
        message: #{message}
      });
    |]


{-
  Joins an existing lobby.

  If the game is in progress, redirects to the game. If an error occurs,
  returns an error page.
-}
getGameLobbyR :: Text -> Handler Html
getGameLobbyR gameId =
  do
    app <- getYesod
    liftIO $ trackRequestReceivedActivity (inactivityTracker app)
    maid <- maybeAuthId
    case maid of
      Just _ -> do
        authedUser <- requireUsername
        handlerLobbyAuthenticated gameId (authenticatedUserId authedUser)
      Nothing -> gamePagelayout $ renderNotLoggedInLobbyPage "Login / Sign Up to Join the Lobby"

handlerLobbyAuthenticated :: Text -> Text -> Handler Html
handlerLobbyAuthenticated gameId userId =
  do
    app <- getYesod
    let playerId = userId
    webSockets $ lobbyWebSocketHandler app gameId playerId

    runResourceT $ do
      (_, lobby) <- getCacheableResource (gameLobbies app) gameId
      case lobby of
        Left _ -> lift $ redirectHandler gameId
        Right gameLobby -> do
          join <- liftIO $ joinClient app gameLobby gameId userId
          invitedPlayers <- liftIO $ getInvitedPlayers (lobbyRepository app) gameId
          lift $ renderLobbyPage join invitedPlayers gameId

redirectHandler :: Text -> Handler Html
redirectHandler gameId = redirect (GameR gameId)

renderLobbyPage :: Either LobbyInputError GL.ClientLobbyJoinResult -> [Text] -> Text -> Handler Html
renderLobbyPage (Left InvalidPlayerID) _ gameId = invalidArgs ["Invalid player ID given by browser"]
renderLobbyPage (Left _) _ gameId = redirectHandler gameId
renderLobbyPage (Right (GL.ClientLobbyJoinResult broadcastChannel (Just gameCreated) _ _)) _ gameId = redirectHandler gameId
renderLobbyPage (Right (GL.ClientLobbyJoinResult broadcastChannel _ _ lobbySnapshot)) invitedPlayers gameId = gamePagelayout $ do
  let joinedPlayerNames = map playerUsername (snapshotLobbyPlayers lobbySnapshot)

  addStylesheet $ (StaticR wordifyCss)
  addScript $ StaticR wordifyJs
  [whamlet|
    <div #lobby>
        
      |]
  toWidget
    [julius|
      var url = document.URL;
      var webSocketUrl = url.replace("http:", "ws:").replace("https:", "wss:");
      
      const lobby = Wordify.createGameLobby('#lobby', {
        playerCount: #{toJSON (snapshotAwaiting lobbySnapshot)},
        gameLobbyId: #{toJSON gameId},
        joinedPlayers: #{toJSON joinedPlayerNames},
        invitedPlayers: #{toJSON invitedPlayers},
        language: #{toJSON (snapShotgameLanguage lobbySnapshot) },
        websocketUrl: webSocketUrl,
        isLoggedIn: true
      });
    |]

lobbyWebSocketHandler :: App -> T.Text -> T.Text -> WebSocketsT Handler ()
lobbyWebSocketHandler app gameId playerId = do
  {-
      We race a thread that sends pings to the client so that when the client closes its connection,
      our loop which reads from the message channel is closed due to the thread being cancelled.
  -}
  connection <- ask
  liftIO $
    withTrackWebsocketActivity (inactivityTracker app) $
      withCacheableResource (gameLobbies app) gameId $ \lobby ->
        case lobby of
          Left err -> putStrLn err >> return ()
          Right lobby -> do
            lobbyChannel <- atomically $ dupTChan (channel lobby)
            race_
              (forever $ atomically (toJSONResponse . handleChannelMessage <$> readTChan lobbyChannel) >>= C.sendTextData connection)
              (forever $ C.sendPing connection ("hello~" :: Text) >> threadDelay 60000000)

newtype LobbyInviteRequest = LobbyInviteRequest { inviteTargetUsername :: Text }

instance FromJSON LobbyInviteRequest where
  parseJSON = withObject "LobbyInviteRequest" $ \obj ->
    LobbyInviteRequest <$> obj .: "inviteTargetUsername"

postGameLobbyR :: Text -> Handler ()
postGameLobbyR gameId = do
  app <- getYesod
  authedUser <- requireUsername
  req <- requireCheckJsonBody :: Handler LobbyInviteRequest
  runResourceT $ do
    (_, eitherLobby) <- getCacheableResource (gameLobbies app) gameId
    case eitherLobby of
      Left _ ->
        lift $ sendStatusJSON status404 (object ["error" .= ("lobby_not_found" :: Text), "message" .= ("The game lobby was not found." :: Text)])
      Right lobby -> do
        result <- liftIO $ sendLobbyInvite
          (lobbyRepository app)
          lobby
          (notificationService app)
          gameId
          (inviteTargetUsername req)
          (authenticatedUserId authedUser)
          (authenticatedUsername authedUser)
        lift $ case result of
          InvitePlayerSuccess _ -> return ()
          InvitedUsernameNotFound -> sendStatusJSON status422 (object ["error" .= ("username_not_found" :: Text), "message" .= ("No user with that username exists." :: Text)])
          InvitedSelf -> sendStatusJSON status422 (object ["error" .= ("cannot_invite_self" :: Text), "message" .= ("You cannot invite yourself to a game." :: Text)])

-- TODO: don't copypasta this and share it somewhere
gamePagelayout :: Widget -> Handler Html
gamePagelayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>Wordify
                    <meta charset="UTF-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    ^{pageHead pc}
                <body>
                    <div .special-wrapper>
                        ^{pageBody pc}
        |]