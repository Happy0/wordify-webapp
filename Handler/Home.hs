module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Widgets.Board.Board
import Widgets.Game.Rack
import Wordify.Rules.Tile
import Widgets.Game.TestGame
import qualified Data.List.NonEmpty as NE
import Wordify.Rules.Board
import Wordify.Rules.Move
import Wordify.Rules.Game
import Widgets.Game.ChatBox
import Widgets.Game.ScoreBoard

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        [whamlet|
            <div>
                <button .create-game-button>Create Game
        |]
        toWidget
            [julius|
                var createGame = function() {
                    alert("Clicked!");
                };

                $(".create-game-button").click(createGame);
            |]
    where
        gameWidget = $(widgetFile "game")
        currentBoard = emptyBoard
        currentPlayers = []
        movesPlayed = []



blah :: Handler Html
blah = do
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

