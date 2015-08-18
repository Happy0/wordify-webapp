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
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text as T
import Network.Mail.Mime
import System.Random
import Controllers.Game.Game
import Controllers.Game.Api
import Model.Api
import Data.Aeson
import Web.Cookie

getHomeR :: Handler Html
getHomeR = do
    webSockets homeWebSocketHandler
    let cookie = def {setCookieName = "blah", setCookieValue = "test"}
    setCookie cookie
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
                                    <p> Your Nickname
                                        <div>
                                            <input #nickname>
                            <div .modal-footer>
                                <button .btn .btn-default .create-game-button type="button">Create</button>

        |]
        toWidget
            [julius|
                var createGameRequest = {
                    "command" : "createGame",
                    "payload" : {
                        "players" : null,
                        "nickname" : null
                    }

                }

                var getNumPlayersSelected = function() {
                    var optionElement = document.getElementById("num-players");
                    return optionElement.options[optionElement.selectedIndex].value;
                };

                var getNickname = function () {
                    return document.getElementById("nickname").value;
                }

                var url = document.URL;
                url = url.replace("http:", "ws:").replace("https:", "wss:");
                var conn = new WebSocket(url);

                conn.onmessage = function(e) {
                    var data = JSON.parse(e.data);

                    parseServerMessage(data);
                };

                var parseServerMessage = function(serverMessage) {
                    console.dir(serverMessage);

                    if (serverMessage && serverMessage.command)
                    {

                        if (serverMessage.command === "gameCreated")
                        {
                            handleGameCreated(serverMessage.payload)
                        }

                    }
                };

                var handleGameCreated = function(payload)
                {
                    console.info("handleGameCreated");

                    var gameId = payload.gameId;

                    window.location = "game" + "/" + gameId;
                };


                var createGameClicked = function() {
                    var playersSelected = getNumPlayersSelected();
                    var nickname = getNickname();

                    createGameRequest.payload.players = parseInt(playersSelected);
                    createGameRequest.payload.nickname = getNickname();
                    var commandText = JSON.stringify(createGameRequest);
                    conn.send(commandText);
                    
                };

                $(".create-game-button").click(createGameClicked);
            |]
    where
        currentBoard = emptyBoard
        currentPlayers = []
        movesPlayed = []
        numPlayerOptions = [2..4]


homeWebSocketHandler :: WebSocketsT Handler ()
homeWebSocketHandler = do
        requestText <- receiveData
        
        let request = decode requestText :: Maybe ClientRequest
        case request of
            Just requestData -> 
                do
                    response <- liftIO $ performRequest requestData
                    sendTextData $ toJSONResponse response
            Nothing -> 
                sendTextData $ "Unrecognised command: " ++ requestText

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

