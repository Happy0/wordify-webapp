module Handler.Game where

import Import
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import qualified Data.Monoid as M
import Data.Text (Text)
import Controllers.Game.Model.ServerGame
import Widgets.Game.Game
import Controllers.Game.Api
import qualified Data.List as L
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Api
import Model.Api
import Control.Concurrent
import qualified Data.List.NonEmpty as NE
import Widgets.Game.TestGame
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import Data.Aeson
import Wordify.Rules.Player
import Controllers.Game.Game

getGameR :: Text -> Handler Html
getGameR gameId = do
    request <- getRequest
    app <- getYesod
    let cookies = reqCookies request
    let maybePlayerId = L.lookup "id" cookies
    let gamesInProgress = games app
    maybeGame <- atomically $ lookup gameId <$> readTVar gamesInProgress

    case maybeGame of
        Nothing -> notFound
        Just gameInProgress ->
            do
                (serverGame, messageChannel) <- atomically $ setupPrerequisets gameInProgress
                let currentGame = game serverGame

                let maybePlayerNumber = (maybePlayerId >>= getPlayerNumber serverGame)
                let maybePlayerRack = tilesOnRack <$> (maybePlayerNumber >>= getPlayer currentGame)

                webSockets $ gameApp gameInProgress messageChannel maybePlayerId maybePlayerNumber
                defaultLayout $ do
                    addStylesheet $ (StaticR css_scrabble_css)
                    addStylesheet $ (StaticR css_round_css)
                    addStylesheet $ (StaticR css_bootstrap_css)
                    addScript $ (StaticR js_round_js)

                    [whamlet|
                        <div #scrabbleground>
                    |]
                    toWidget
                        [julius|
                            var url = document.URL,

                            url = url.replace("http:", "ws:").replace("https:", "wss:");
                            var conn = new WebSocket(url);

                            var send = function(objectPayload) {
                                var json = JSON.stringify(objectPayload);
                                conn.send(json);
                            }

                            var opts = {element: document.getElementById("scrabbleground")};
                            opts.ground = {}
                            opts.ground.board = #{toJSON (board currentGame)};
                            opts.send = send;
                            var round = Round(opts);

                            round.controller.setPlayers( #{toJSON (players currentGame)});
                            round.controller.setRackTiles(#{toJSON (maybePlayerRack)});
                            round.controller.setPlayerNumber(#{toJSON maybePlayerNumber});
                            round.controller.setPlayerToMove(#{toJSON (playerNumber currentGame)});
                            round.controller.setTilesRemaining(#{toJSON (bagSize (bag currentGame))})

                            conn.onmessage = function (e) {
                                var data = JSON.parse(e.data);
                                round.socketReceive(data);
                            }

                        |]

getPlayerNumber :: ServerGame -> Text -> Maybe Int
getPlayerNumber serverGame playerId = fst <$> (L.find (\(ind, player) -> playerId == identifier player) $ zip [1 .. 4] players)
    where
        players = playing serverGame

setupPrerequisets :: TVar ServerGame -> STM (ServerGame, TChan GameMessage)
setupPrerequisets serverGame =
    do
        game <- readTVar serverGame
        channel <- dupTChan $ broadcastChannel game
        return (game, channel)


gameApp :: TVar ServerGame -> TChan GameMessage -> Maybe Text -> Maybe Int -> WebSocketsT Handler ()
gameApp game channel maybePlayerId playerNumber =
        race_
            (forever $ atomically (readTChan channel) >>= sendTextData . toJSONResponse)
            (forever $
                do
                    msg <- receiveData
                    case eitherDecode msg of
                        Left err -> sendTextData $ toJSONResponse (InvalidCommand (pack err))
                        Right parsedCommand -> do
                            response <- liftIO $ performRequest game playerNumber parsedCommand
                            sendTextData . toJSONResponse $ response
            )
