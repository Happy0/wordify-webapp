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
import Wordify.Rules.Board
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
       maybeGame <- atomically $ do
            game <- (lookup gameId <$> readTVar gameCache)
            case game of
                Just cachedGame ->
                    modifyTVar (numConnections cachedGame) (+1) >> return game
                _ -> return game

       case maybeGame of
            Nothing -> do
                dbResult <- loadFromDatabase pool dictionary letterBag gameId gameCache
                case dbResult of
                    Left err -> return $ Left err
                    Right (spawnDbListener, gm) -> 
                        do 
                            spawnDbListener
                            return $ Right gm
            Just game -> return $ Right game

{-
    Loads a game from the database into the game cache. Returns the loaded game and
    an action which spawns a thread to listen to game events and write them to the database.
    If the game has already been loaded into the cache, the returned action does nothing.
-}
loadFromDatabase :: Pool SqlBackend -> 
                    Dictionary ->
                    LetterBag ->
                    Text ->
                    TVar (Map Text ServerGame) ->
                    IO (Either Text (IO (), ServerGame))
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
                            modifyTVar (numConnections serverGame) (+1)
                            writeTVar gameCache newCache
                            channel <- duplicateGameChannel serverGame
                            let spawnDbListener = (forkIO $ watchForUpdates pool gameId channel) >> return ()

                            return $ Right (spawnDbListener, serverGame)
                        Just entry -> do
                            modifyTVar (numConnections entry) (+1)
                            channel <- duplicateGameChannel entry
                            let spawnDbListener = return ()
                            return $ Right (spawnDbListener, entry)

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

    webSockets $ gameApp app gameId maybePlayerId

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
                opts.ground.board = #{toJSON emptyBoard};
                opts.send = send;
                var round = Round(opts);

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

gameApp :: App -> Text -> Maybe Text -> WebSocketsT Handler ()
gameApp app gameId maybePlayerId = do
    connection <- ask
    liftIO $ bracket
        (getGameDefaultLocale app gameId)
        releaseGame
        (keepClientUpdated app gameId connection maybePlayerId)

    where
        releaseGame :: Either Text ServerGame -> IO ()
        releaseGame g =
            case g of
                Right g ->
                    atomically $ do
                        modifyTVar (numConnections g) (\connections -> connections - 1)
                        connections <- readTVar $ numConnections g

                        if connections == 0
                            then modifyTVar (games app) (M.delete gameId)
                            else return ()
                Left _ -> return ()

getGameDefaultLocale :: App -> Text -> IO (Either Text ServerGame)
getGameDefaultLocale app gameId = do
    let (dictionary, letterBag) = getDictionaryAndLetterBag app
    game <- loadGame (appConnPool app) dictionary letterBag gameId (games app)
    return game

keepClientUpdated :: App -> Text -> C.Connection -> Maybe Text -> Either Text ServerGame -> IO ()
keepClientUpdated app gameId connection maybePlayerId g = do
       case g of
            Left initError ->
                C.sendTextData connection $ toJSONResponse (InvalidCommand initError)
            Right serverGame -> do
                (channel, gameSoFar) <- atomically $ do
                        channel <- duplicateGameChannel serverGame
                        gameSoFar <- readTVar (game serverGame)
                        return (channel, gameSoFar)

                let playerNumber = maybePlayerId >>= (getPlayerNumber serverGame)

                sendOriginalGameState connection playerNumber gameSoFar
                sendPreviousTransitions connection gameSoFar
                sendPreviousChatMessages (appConnPool app) gameId connection

                race_
                        (forever $
                            atomically (readTChan channel) >>= \message -> (C.sendTextData connection $ toJSONResponse message))
                        (forever $
                            do
                                msg <- C.receiveData connection
                                case eitherDecode msg of
                                    Left err -> C.sendTextData connection $ toJSONResponse (InvalidCommand (pack err))
                                    Right parsedCommand -> do
                                        response <- liftIO $ performRequest serverGame playerNumber parsedCommand
                                        C.sendTextData connection $ toJSONResponse $ response
                        )
{-
    case maybeGame of
        Left err -> invalidArgs [err]
        Right serverGame ->
            do
                (currentGame, messageChannel) <-
                     atomically $ do
                        channel <- duplicateGameChannel serverGame
                        currentGame <- readTVar $ game serverGame
                        return (currentGame, channel)
-}

sendOriginalGameState :: C.Connection -> Maybe Int -> G.Game -> IO ()
sendOriginalGameState connection maybePlayerNumber game =
    do
        let rack = tilesOnRack <$> (maybePlayerNumber >>= G.getPlayer game)
        let players = G.players game
        let playing = G.playerNumber game
        let numTilesRemaining = (bagSize (G.bag game))
        C.sendTextData connection $
            toJSONResponse $ InitialiseGame rack players (maybePlayerNumber) playing numTilesRemaining


sendPreviousTransitions :: C.Connection -> G.Game -> IO ()
sendPreviousTransitions connection game =
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
                            Right transition -> C.sendTextData connection $ toJSONResponse (transitionToMessage transition)
        where
            (G.History originalBag moves) = G.history game

{-
    Send the chat log history to the client
-}
sendPreviousChatMessages :: Pool SqlBackend -> Text -> C.Connection -> IO ()
sendPreviousChatMessages pool gameId connection = do
    liftIO $ flip runSqlPersistMPool pool $
        getChatMessages gameId $$ CL.map toJSONResponse $= (CL.mapM_ (liftIO . C.sendTextData connection))
