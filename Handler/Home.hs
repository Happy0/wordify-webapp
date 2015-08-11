module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Widgets.Game.Board
import Widgets.Game.Rack
import Wordify.Rules.Tile
import Widgets.Game.TestGame
import qualified Data.List.NonEmpty as NE
import Wordify.Rules.Board
import Wordify.Rules.Move
import Wordify.Rules.Game
import Widgets.Game.ChatBox
import Widgets.Game.Game
import Widgets.Game.ScoreBoard

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        [whamlet|
            <div>
                <button .btn .btn-info .btn-lg data-toggle="modal" data-target="#create-game-lobby">Create Game
                ^{emptyGame}

                <div .modal .fade #create-game-lobby role="dialog">
                    <div .modal-dialog>
                        <div .modal-content>
                            <div .modal-header>
                                <button .close type="button" data-dismiss="modal">&times;
                                <h4 class="modal-title">Create Game
                            <div .modal-body>
                                <div>
                                    <p> Number of Players
                                    <select #num-players>
                                        $forall i <- numPlayerOptions
                                            <option value="#{show i}"> #{show i}
                            <div .modal-footer>
                                <button .btn .btn-default .create-game-button type="button">Create</button>

        |]
        toWidget
            [julius|
                var getNumPlayersSelected = function() {
                    var optionElement = document.getElementById("num-players");
                    return optionElement.options[optionElement.selectedIndex].value;
                };

                var createGameClicked = function() {
                    var playersSelected = getNumPlayersSelected();
                    alert(playersSelected);
                };

                $(".create-game-button").click(createGameClicked);
            |]
    where
        currentBoard = emptyBoard
        currentPlayers = []
        movesPlayed = []
        numPlayerOptions = [2..4]




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
                in let tiles = []
                in $(widgetFile "game")

