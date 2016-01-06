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
import qualified Wordify.Rules.Game as G
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import Wordify.Rules.Dictionary
import Data.Aeson
import Wordify.Rules.Player
import Controllers.Game.Game
import Controllers.Game.Persist
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

loadGame :: Pool SqlBackend -> Dictionary -> LetterBag -> Text -> TVar (Map Text ServerGame) -> IO (Either Text ServerGame)
loadGame pool dictionary letterBag gameId gameCache =
    do
        maybeGame <- atomically $ (lookup gameId <$> readTVar gameCache)
        case maybeGame of
            Nothing -> loadFromDatabase pool dictionary letterBag gameId gameCache
            Just game -> return $ Right game

loadFromDatabase :: Pool SqlBackend -> Dictionary -> LetterBag -> Text -> TVar (Map Text ServerGame) -> IO (Either Text ServerGame)
loadFromDatabase pool dictionary letterBag gameId gameCache =
    do
        eitherGame <- getGame pool letterBag dictionary gameId
        case eitherGame of
            Left err -> return $ Left err
            Right serverGame ->
                -- Add the game to the cache of active games
                atomically $ do
                    -- Check another client didn't race us to the database
                    cachedGame <- lookup gameId <$> readTVar gameCache
                    case cachedGame of
                        Nothing -> do
                            newCache <- M.insert gameId serverGame <$> readTVar gameCache
                            writeTVar gameCache newCache
                            return $ Right serverGame
                        Just entry -> do
                            return $ Right entry

getDictionaryAndLetterBag :: App -> (Dictionary, LetterBag)
getDictionaryAndLetterBag app = do
            let setups = localisedGameSetups app
            -- Don't taze me bro, will fix later
            let Just setup = M.lookup "en" setups
            (localisedDictionary setup, localisedLetterBag setup)

getGameR :: Text -> Handler Html
getGameR gameId = do
    request <- getRequest
    app <- getYesod
    let cookies = reqCookies request
    let maybePlayerId = L.lookup "id" cookies
    let gamesInProgress = games app

    let (dictionary, letterBag) = getDictionaryAndLetterBag app
    maybeGame <- liftIO $ loadGame (appConnPool app) dictionary letterBag gameId gamesInProgress

    case maybeGame of
        Left err -> invalidArgs [err]
        Right serverGame ->
            do
                (currentGame, messageChannel) <-
                     atomically $ do
                        channel <- duplicateGameChannel serverGame
                        currentGame <- readTVar $ game serverGame
                        return (currentGame, channel)

                let maybePlayerNumber = (maybePlayerId >>= getPlayerNumber serverGame)
                let maybePlayerRack = tilesOnRack <$> (maybePlayerNumber >>= G.getPlayer currentGame)

                webSockets $ gameApp app gameId currentGame serverGame messageChannel maybePlayerId maybePlayerNumber
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
                            opts.ground.board = #{toJSON (G.board currentGame)};
                            opts.send = send;
                            var round = Round(opts);

                            round.controller.setPlayers( #{toJSON (G.players currentGame)});
                            round.controller.setRackTiles(#{toJSON (maybePlayerRack)});
                            round.controller.setPlayerNumber(#{toJSON maybePlayerNumber});
                            round.controller.setPlayerToMove(#{toJSON (G.playerNumber currentGame)});
                            round.controller.setTilesRemaining(#{toJSON (bagSize (G.bag currentGame))});

                            conn.onmessage = function (e) {
                                var data = JSON.parse(e.data);
                                round.socketReceive(data);
                            }

                        |]

getPlayerNumber :: ServerGame -> Text -> Maybe Int
getPlayerNumber serverGame playerId = fst <$> (L.find (\(ind, player) -> playerId == identifier player) $ zip [1 .. 4] players)
    where
        players = playing serverGame

duplicateGameChannel :: ServerGame -> STM (TChan GameMessage)
duplicateGameChannel serverGame = dupTChan $ broadcastChannel serverGame

gameApp :: App -> Text -> G.Game -> ServerGame -> TChan GameMessage -> Maybe Text -> Maybe Int -> WebSocketsT Handler ()
gameApp app gameId initialGame sharedGame channel maybePlayerId playerNumber = do
       connection <- ask
       sendPreviousTransitions initialGame
       liftIO $ sendPreviousChatMessages (appConnPool app) gameId connection
       race_
            (forever $ atomically (readTChan channel) >>= sendTextData . toJSONResponse)
            (forever $
                do
                    msg <- receiveData
                    case eitherDecode msg of
                        Left err -> sendTextData $ toJSONResponse (InvalidCommand (pack err))
                        Right parsedCommand -> do
                            response <- liftIO $ performRequest sharedGame playerNumber parsedCommand
                            sendTextData . toJSONResponse $ response
            )

sendPreviousTransitions :: G.Game -> WebSocketsT Handler ()
sendPreviousTransitions game =
    do
        let moves = G.movesMade game

        -- TODO: Add function to make a new empty game from a game history to
        -- haskellscrabble

        let Right playersState = makeGameStatePlayers (L.length $ G.players game)

        let Right emptyGame = G.makeGame playersState originalBag (G.dictionary game)
        if (length moves == 0) then
            return ()
            else do
                let gameTransitions = restoreGameLazy emptyGame $ NE.fromList moves

                flip mapM_ gameTransitions $
                    \transition ->
                        case transition of
                            Left err -> liftIO $ putStrLn . pack . show $ err
                            Right transition -> sendTextData $ toJSONResponse (transitionToMessage transition)
        where
            (G.History originalBag moves) = G.history game

{-
    Send the chat log history to the client
-}
sendPreviousChatMessages :: Pool SqlBackend -> Text -> C.Connection -> IO ()
sendPreviousChatMessages pool gameId connection = do
    liftIO $ flip runSqlPersistMPool pool $
        getChatMessages gameId $$ CL.map toJSONResponse $= (CL.mapM_ (liftIO . C.sendTextData connection))





