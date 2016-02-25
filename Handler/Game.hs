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

    let (dictionary, letterBag) = getDictionaryAndLetterBag app

    withGame app gameId dictionary letterBag (handleGameResult app dictionary letterBag gameId maybePlayerId)

handleGameResult :: App -> Dictionary -> LetterBag -> Text -> Maybe Text -> Either Text ServerGame -> Handler Html
handleGameResult _ _ _ _ _ (Left err) = invalidArgs [err]
handleGameResult app dictionary letterBag gameId maybePlayerId (Right serverGame) = do
  let maybePlayerNumber = maybePlayerId >>= (getPlayerNumber serverGame)

  {-- If this is a websocket request, the handler is short cutted here
      Once the client has loaded the page and javascript, the javascript for the page
      initiates the websocket request which arrives here -}
  webSockets $ gameApp app gameId dictionary letterBag maybePlayerNumber

  gameSoFar <- liftIO (readTVarIO (game serverGame))
  let rack = tilesOnRack <$> (maybePlayerNumber >>= G.getPlayer gameSoFar)
  let players = G.players gameSoFar
  let playing = G.playerNumber gameSoFar
  let numTilesRemaining = (bagSize (G.bag gameSoFar))
  let gameMoveSummaries = gameToMoveSummaries gameSoFar

  case gameMoveSummaries of
    Left err -> invalidArgs [err]
    Right _ -> liftIO (return ())

  let summaries = either (\_ -> []) id gameMoveSummaries

  defaultLayout $ do
      addStylesheet $ (StaticR css_scrabble_css)
      addStylesheet $ (StaticR css_round_css)
      addStylesheet $ (StaticR css_bootstrap_css)
      addScript $ (StaticR js_round_js)
      toWidget
          [julius|
              jQuery.noConflict();

              var url = document.URL,

              url = url.replace("http:", "ws:").replace("https:", "wss:");
              var conn = new WebSocket(url);

              var send = function(objectPayload) {
                  var json = JSON.stringify(objectPayload);
                  conn.send(json);
              }

              var opts = {element: document.getElementById("scrabbleground")};
              opts.ground = {}
              opts.ground.board = #{toJSON (G.board gameSoFar)};
              opts.players = #{toJSON players}
              opts.playerNumber = #{toJSON maybePlayerNumber}
              opts.playerToMove = #{toJSON playing}
              opts.tilesRemaining = #{toJSON numTilesRemaining}
              opts.moveHistory = #{toJSON summaries}

              opts.send = send;
              var round = Round(opts);

              //TODO: Make a rack be definable in the 'data' object
              round.controller.updateRack(#{toJSON rack});

              conn.onmessage = function (e) {
                  var data = JSON.parse(e.data);
                  round.socketReceive(data);
              };

              document.addEventListener('DOMContentLoaded', function () {
                if (Notification.permission !== "granted")
                  Notification.requestPermission();
              });
          |]
      [whamlet|
          <div #scrabbleground>
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

gameApp :: App -> Text -> Dictionary -> LetterBag -> Maybe Int -> WebSocketsT Handler ()
gameApp app gameId dictionary letterBag maybePlayerNumber = do
    connection <- ask
    withGame app gameId dictionary letterBag $ \result ->
      case result of
        Left err -> sendTextData (toJSONResponse (InvalidCommand err))
        Right serverGame -> do
          channel <- atomically (cloneTChan (broadcastChannel serverGame))
          liftIO (keepClientUpdated connection (appConnPool app) gameId serverGame channel maybePlayerNumber)

keepClientUpdated :: C.Connection -> ConnectionPool -> Text -> ServerGame -> TChan GameMessage -> Maybe Int -> IO ()
keepClientUpdated connection pool gameId serverGame channel playerNumber = do
    sendPreviousChatMessages pool gameId connection

    -- Send the moves again incase the client missed any inbetween loading the page and
    -- connecting with the websocket
    readTVarIO (game serverGame) >>= sendPreviousMoves connection

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

sendPreviousMoves :: C.Connection -> G.Game -> IO ()
sendPreviousMoves connection game = do
  if (length moves == 0) then
    return ()
    else
      do
        let transitions = restoreGameLazy emptyGame $ NE.fromList (toList moves)
        forM_ (NE.toList transitions) $ \transition ->
          case transition of
            Left err -> C.sendTextData connection $ toJSONResponse (InvalidCommand (pack (show err)))
            Right transition ->
              C.sendTextData connection $ toJSONResponse (transitionToMessage transition)
  where
    -- TODO: Add function to make a new empty game from a game history to
    -- haskellscrabble
    Right playersState = makeGameStatePlayers (L.length $ G.players game)
    Right emptyGame = G.makeGame playersState originalBag (G.dictionary game)
    (G.History originalBag moves) = G.history game


gameToMoveSummaries :: G.Game -> Either Text [MoveSummary]
gameToMoveSummaries game =
      if (length moves /= 0) then do
        let gameTransitions = restoreGameLazy emptyGame $ NE.fromList (toList moves)
        let reconstructedGameSummaries = sequence . map (fmap transitionToSummary) $ NE.toList gameTransitions
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

getDictionaryAndLetterBag :: App -> (Dictionary, LetterBag)
getDictionaryAndLetterBag app = do
            let setups = localisedGameSetups app
            -- Don't taze me bro, will fix later
            let Just setup = M.lookup "en" setups
            (localisedDictionary setup, localisedLetterBag setup)
