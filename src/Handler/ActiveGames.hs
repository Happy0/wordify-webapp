module Handler.ActiveGames where

import ClassyPrelude.Yesod
import qualified Data.Text as T
import Foundation
import Foundation (App (appConnPool), Handler)
import Import (whamlet)
import Repository.GameRepository
import Repository.SQL.SqlGameRepository (GameRepositorySQLBackend (GameRepositorySQLBackend))
import Yesod.Auth

renderNotLoggedInPage =
  defaultLayout $
    [whamlet|
            <p> You must log in to view this page
            <a href=@{AuthR LoginR}>Login
        |]

renderActiveGamePage :: (GameRepository a) => a -> T.Text -> Handler Html
renderActiveGamePage gameRepository userId = do
  activeGames <- liftIO $ getActiveUserGames gameRepository userId
  defaultLayout $
    toWidget $
      [whamlet|
                <p> Shut it... This totally an acceptable rendering of a game list... YOU'RE a bad front end developer
                $forall GameSummary gameId latestActivity _ <- activeGames
                    <p><a href=@{GameR gameId}> #{gameId}
                |]

getActiveGamesR :: Handler Html
getActiveGamesR = do
  app <- getYesod
  let pool = appConnPool app
  let gameRepositorySQLBackend = GameRepositorySQLBackend pool
  maybePlayerId <- maybeAuthId

  case maybePlayerId of
    Nothing -> renderNotLoggedInPage
    Just userId -> renderActiveGamePage gameRepositorySQLBackend userId