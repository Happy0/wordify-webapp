module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Wordify.Rules.Tile
import qualified Data.List.NonEmpty as NE
import Wordify.Rules.Board
import Wordify.Rules.Move
import Wordify.Rules.Game
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text as T
import Network.Mail.Mime
import System.Random
import Controllers.Home.Home
import Controllers.Home.Api
import Model.Api
import Data.Aeson
import Web.Cookie

getHomeR :: Handler Html
getHomeR = do
    webSockets homeWebSocketHandler
    let cookie = def {setCookieName = "blah", setCookieValue = "test"}
    withCreateGameDialog <- isJust <$> (lookupGetParam "create")

    setCookie cookie
    defaultLayout $ do
        [whamlet|
            <div style="width: 150px; height 50px; margin: auto; margin-bottom: 200px;">

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
                var createGameRequest = {
                    "command" : "createGame",
                    "payload" : {
                        "players" : null
                    }
                }

                if (#{withCreateGameDialog}) {
                  $("#create-game-lobby").modal();
                }

                var getNumPlayersSelected = function() {
                    var optionElement = document.getElementById("num-players");
                    return optionElement.options[optionElement.selectedIndex].value;
                };

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

                    window.location = "game" + "/" + gameId + "/lobby";
                };


                var createGameClicked = function() {
                    var playersSelected = getNumPlayersSelected();

                    createGameRequest.payload.players = parseInt(playersSelected);
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
        app <- getYesod
        requestText <- receiveData

        let request = eitherDecode requestText :: Either String ClientRequest
        case request of
            Right requestData ->
                do
                    result <- liftIO $ performRequest app requestData
                    case result of
                            Left clientError ->
                                sendTextData $ toJSONResponse $ ClientError clientError
                            Right requestResponse ->
                                sendTextData $ toJSONResponse requestResponse
            Left reason ->
                sendTextData $ toJSONResponse (ClientError $ T.pack reason)
