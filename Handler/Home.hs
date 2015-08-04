module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Widgets.Board.Board
import Widgets.Game.Rack
import Wordify.Rules.Tile
import Widgets.Game.TestGame
import qualified Data.List.NonEmpty as NE
import Wordify.Rules.Move
import Wordify.Rules.Game
import Widgets.Game.ChatBox
import Widgets.Game.ScoreBoard

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Wordify"
        game <- lift $ testGame
        let neMoves = NE.fromList moves
        let fullGame = join $ fmap (flip (restoreGame) neMoves) $ game

        case fullGame of
            Left err -> [whamlet|Hello World! #{(show err)}|]
            Right gameTransitions ->
                let currentGame = newGame $ NE.last gameTransitions
                in let currentPlayers = players currentGame
                in let movesPlayed = NE.toList gameTransitions
                in let currentBoard = (board currentGame)
                in $(widgetFile "game")


