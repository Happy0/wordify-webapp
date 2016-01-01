module Handler.Game where

import Data.Pool
import Database.Persist.Sql
import qualified Network.WebSockets.Connection as C
import Import
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import qualified Data.Monoid as M
import Data.Text (Text)
import Controllers.Game.Model.ServerGame
import Controllers.Game.Api
import qualified Data.List as L
import Controllers.Game.Model.ServerPlayer
import Controllers.Game.Api
import Controllers.Game.Persist
import Data.Conduit
import qualified Data.Conduit.List as CL
import Model.Api
import Control.Concurrent
import qualified Data.List.NonEmpty as NE
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
                let moveHistory = moveSummaries serverGame

                webSockets $ gameApp app gameId gameInProgress messageChannel maybePlayerId maybePlayerNumber
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
                            round.controller.setTilesRemaining(#{toJSON (bagSize (bag currentGame))});
                            round.controller.setMoveHistory(#{toJSON moveHistory});

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


gameApp :: App -> Text -> TVar ServerGame -> TChan GameMessage -> Maybe Text -> Maybe Int -> WebSocketsT Handler ()
gameApp app gameId game channel maybePlayerId playerNumber = do
       connection <- ask
       liftIO $ sendPreviousChatMessages (appConnPool app) gameId connection
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

{-
    Send the chat log history to the client
-}
sendPreviousChatMessages :: Pool SqlBackend -> Text -> C.Connection -> IO ()
sendPreviousChatMessages pool gameId connection = do
    liftIO $ flip runSqlPersistMPool pool $
        getChatMessages gameId $$ CL.map toJSONResponse $= (CL.mapM_ (liftIO . C.sendTextData connection))





