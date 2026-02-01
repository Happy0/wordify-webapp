module Handler.Home where

import qualified Data.Map as M
import ClassyPrelude.Yesod
import qualified Data.Text as T
import Foundation
import Foundation (App (appConnPool), Handler)
import Import (whamlet)
import Repository.GameRepository
import Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend))
import Yesod.Auth
import Import.NoFoundation (css_wordify_css)
import Import.NoFoundation (js_wordify_js)

type TileValues = Map Text Int

data ActiveGameSummary = ActiveGameSummary {gameId :: Text, boardString:: Text,yourMove :: Bool, lastActivity :: Maybe UTCTime, tileValues :: TileValues}

instance ToJSON ActiveGameSummary where
  toJSON (ActiveGameSummary gameId boardString yourMove lastActivity tileValues) = object [ 
    "gameId" .= gameId,
    "boardString" .= boardString,
    "yourMove" .= yourMove,
    "lastActivity" .= lastActivity,
    "tileValues" .= tileValues
     ]

getTileValues :: App -> Text -> TileValues
getTileValues app locale = M.empty

mapGameSummary :: App -> GameSummary -> ActiveGameSummary
mapGameSummary app (GameSummary gameId latestActivity myMove boardString locale) = ActiveGameSummary gameId boardString myMove latestActivity (getTileValues app locale)

renderNotLoggedInPage :: Handler Html
renderNotLoggedInPage =
  gamePagelayout $ do
    addStylesheet $ (StaticR css_wordify_css)
    addScript $ StaticR js_wordify_js
    [whamlet|
      <div #home>
          
        |]
    toWidget
      [julius|
        const lobby = Wordify.createHome('#home', {
          isLoggedIn: false,
          games: [],
          tileValues: {}
        });
      |]

renderPlayerMoveNote :: Bool -> Widget
renderPlayerMoveNote False = [whamlet| <span> |]
renderPlayerMoveNote True = [whamlet| <span> (Your move) |]

renderActiveGamePage :: (GameRepository a) => App -> a -> T.Text -> Handler Html
renderActiveGamePage app gameRepository userId = do
  activeGames <- liftIO $ getActiveUserGames gameRepository userId
  gamePagelayout $ do
    addStylesheet $ (StaticR css_wordify_css)
    addScript $ StaticR js_wordify_js
    [whamlet|
      <div #home>
          
        |]
    toWidget
      [julius|
        const lobby = Wordify.createHome('#home', {
          isLoggedIn: true,
          games: #{toJSON (map (mapGameSummary app) activeGames)},
          tileValues: {}
        });
      |]

getHomeR :: Handler Html
getHomeR = do
  app <- getYesod
  let pool = appConnPool app
  let gameRepositorySQLBackend = GameRepositorySQLBackend pool
  maybePlayerId <- maybeAuthId

  case maybePlayerId of
    Nothing -> renderNotLoggedInPage
    Just userId -> renderActiveGamePage app gameRepositorySQLBackend userId

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