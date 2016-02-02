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

getGameDebugR :: Handler Html
getGameDebugR = do
    app <- getYesod
    gameSize <- liftIO $ M.size <$> (readTVarIO $ games app)
    lobbiesSize <- liftIO $ M.size <$> (readTVarIO $ gameLobbies app)
    defaultLayout $ do
        [whamlet|
            <div> Games: #{gameSize}
            <div> Lobbies: #{lobbiesSize}
        |]

gameApp :: App -> Text -> Maybe Text -> WebSocketsT Handler ()
gameApp app gameId maybePlayerId = do
    connection <- ask
    bracketLiftIO
        (getGameDefaultLocale app gameId)
        releaseGame
        (keepClientUpdated app gameId connection maybePlayerId)
    where
        bracketLiftIO acquire release run = liftIO (bracket acquire release run)

        releaseGame :: Either Text ServerGame -> IO ()
        releaseGame (Left _) = return ()
        releaseGame (Right game) =
            atomically $ do
                modifyTVar (numConnections game) (\connections -> connections - 1)
                connections <- readTVar $ numConnections game

                if connections == 0
                    -- Tell the thread which writes updates to the database to finish writing
                    -- the messages and then remove the game from the cache if there are still
                    -- no connected players
                    then writeTChan (broadcastChannel game) GameIdle
                    else return ()

keepClientUpdated :: App -> Text -> C.Connection -> Maybe Text -> Either Text ServerGame -> IO ()
keepClientUpdated _ _ connection _ (Left initialisationError) =
    let errorResponseText = toJSONResponse (InvalidCommand initialisationError)
    in C.sendTextData connection errorResponseText

keepClientUpdated app gameId connection maybePlayerId (Right serverGame) = do
    (channel, gameSoFar) <- atomically $ do
            channel <- duplicateGameChannel serverGame
            gameSoFar <- readTVar (game serverGame)
            return (channel, gameSoFar)

    let playerNumber = maybePlayerId >>= (getPlayerNumber serverGame)

    sendOriginalGameState connection playerNumber gameSoFar
    sendPreviousChatMessages (appConnPool app) gameId connection

    race_
            (forever $
                (atomically . readTChan) channel >>= C.sendTextData connection . toJSONResponse)
            (forever $
                do
                    msg <- C.receiveData connection
                    case eitherDecode msg of
                        Left err -> C.sendTextData connection $ toJSONResponse (InvalidCommand (pack err))
                        Right parsedCommand -> do
                            response <- liftIO $ performRequest serverGame playerNumber parsedCommand
                            C.sendTextData connection $ toJSONResponse $ response
            )

sendOriginalGameState :: C.Connection -> Maybe Int -> G.Game -> IO ()
sendOriginalGameState connection maybePlayerNumber game =
    do
        let rack = tilesOnRack <$> (maybePlayerNumber >>= G.getPlayer game)
        let players = G.players game
        let playing = G.playerNumber game
        let numTilesRemaining = (bagSize (G.bag game))
        let gameTransitionCommands = gameToTransitionCommands game

        case gameTransitionCommands of
          Left err -> C.sendTextData connection (toJSONResponse (InvalidCommand err))
          Right transitionCommands ->
            C.sendTextData connection $
                toJSONResponse $ InitialiseGame transitionCommands rack players (maybePlayerNumber) playing numTilesRemaining

gameToTransitionCommands :: G.Game -> Either Text [GameMessage]
gameToTransitionCommands game =
      if (length moves /= 0) then do
        let gameTransitions = restoreGameLazy emptyGame $ NE.fromList (toList moves)
        let reconstructedGameSummaries = sequence . map (fmap transitionToMessage) $ NE.toList gameTransitions
        case reconstructedGameSummaries of
          Left err -> Left (pack (show err))
          Right summaries -> Right summaries
      else return []
      where

          -- TODO: Add function to make a new empty game from a game history to
          -- haskellscrabble
          Right playersState = makeGameStatePlayers (L.length $ G.players game)
          Right emptyGame = G.makeGame playersState originalBag (G.dictionary game)
          (G.History originalBag moves) = G.history game

{-
    Send the chat log history to the client
-}
sendPreviousChatMessages :: Pool SqlBackend -> Text -> C.Connection -> IO ()
sendPreviousChatMessages pool gameId connection = do
    liftIO $ flip runSqlPersistMPool pool $
        getChatMessages gameId $$ CL.map toJSONResponse $= (CL.mapM_ (liftIO . C.sendTextData connection))

getPlayerNumber :: ServerGame -> Text -> Maybe Int
getPlayerNumber serverGame playerId = fst <$> (L.find (\(ind, player) -> playerId == identifier player) $ zip [1 .. 4] players)
    where
        players = playing serverGame

duplicateGameChannel :: ServerGame -> STM (TChan GameMessage)
duplicateGameChannel serverGame = cloneTChan $ broadcastChannel serverGame

getGameDefaultLocale :: App -> Text -> IO (Either Text ServerGame)
getGameDefaultLocale app gameId = do
    let (dictionary, letterBag) = getDictionaryAndLetterBag app
    game <- loadGame app dictionary letterBag gameId
    return game

getDictionaryAndLetterBag :: App -> (Dictionary, LetterBag)
getDictionaryAndLetterBag app = do
            let setups = localisedGameSetups app
            -- Don't taze me bro, will fix later
            let Just setup = M.lookup "en" setups
            (localisedDictionary setup, localisedLetterBag setup)
