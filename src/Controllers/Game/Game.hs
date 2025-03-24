module Controllers.Game.Game
  ( performRequest,
    notifyGameConnectionStatus,
  )
where

import ClassyPrelude (getCurrentTime, traverse_, whenM)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception (bracket_)
import Control.Monad
import Control.Monad.STM
import Controllers.Game.Api
import Controllers.Game.Model.ServerGame
import qualified Controllers.Game.Model.ServerPlayer as SP
import qualified Controllers.Game.Persist as P
import Controllers.User.Model.AuthUser
import Data.Conduit
import qualified Data.Map as M
import Data.Pool
import Data.Text
import Data.Time
import Database.Persist.Sql
import Model (User)
import Wordify.Rules.FormedWord
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import Wordify.Rules.Player
import Wordify.Rules.Pos
import Wordify.Rules.ScrabbleError
import Wordify.Rules.Tile
import Prelude

handlePlayerConnect :: ServerGame -> Maybe AuthUser -> IO ()
handlePlayerConnect serverGame Nothing = pure ()
handlePlayerConnect serverGame (Just user) = do
  now <- getCurrentTime
  atomically $ handleNewPlayerConnection serverGame user now
  where
    handleNewPlayerConnection :: ServerGame -> AuthUser -> UTCTime -> STM ()
    handleNewPlayerConnection serverGame user now = do
      let connectionCount = increasePlayerConnections serverGame user now
      whenM (isFirstConnection <$> connectionCount) $ notifyPlayerConnect serverGame user

    isFirstConnection :: Maybe Int -> Bool
    isFirstConnection (Just connectionCount) = connectionCount == 1
    isFirstConnection Nothing = False

handlePlayerDisconnect :: ServerGame -> Maybe AuthUser -> IO ()
handlePlayerDisconnect serverGame Nothing = pure ()
handlePlayerDisconnect serverGame (Just user) = do
  now <- getCurrentTime
  atomically $ handleNewPlayerConnection serverGame user now
  where
    handleNewPlayerConnection :: ServerGame -> AuthUser -> UTCTime -> STM ()
    handleNewPlayerConnection serverGame user now = do
      let connectionCount = increasePlayerConnections serverGame user now
      whenM (isLastConnection <$> connectionCount) $ notifyPlayerDisconnect serverGame user

    isLastConnection :: Maybe Int -> Bool
    isLastConnection (Just connectionCount) = connectionCount == 0
    isLastConnection Nothing = False

notifyPlayerConnect :: ServerGame -> AuthUser -> STM ()
notifyPlayerConnect serverGame user = do
  let playerNumber = getPlayerNumber serverGame user
  traverse_ (writeTChan (broadcastChannel serverGame) . PlayerConnect) playerNumber

notifyPlayerDisconnect :: ServerGame -> AuthUser -> STM ()
notifyPlayerDisconnect serverGame user = do
  let playerNumber = getPlayerNumber serverGame user
  traverse_ (writeTChan (broadcastChannel serverGame) . PlayerDisconnect) playerNumber

notifyGameConnectionStatus :: ServerGame -> Maybe AuthUser -> IO () -> IO ()
notifyGameConnectionStatus serverGame maybeUser = bracket_ (handlePlayerConnect serverGame maybeUser) (handlePlayerDisconnect serverGame maybeUser)

performRequest :: ServerGame -> Pool SqlBackend -> Maybe AuthUser -> ClientMessage -> IO ServerResponse
performRequest serverGame pool player (BoardMove placed) = handleBoardMove serverGame pool (player >>= getPlayerNumber serverGame) placed
performRequest serverGame pool player (ExchangeMove exchanged) = handleExchangeMove serverGame pool (player >>= getPlayerNumber serverGame) exchanged
performRequest serverGame pool player PassMove = handlePassMove serverGame pool (player >>= getPlayerNumber serverGame)
performRequest serverGame pool player (SendChatMessage msg) = handleChatMessage serverGame pool player msg
performRequest serverGame pool player (AskPotentialScore placed) = handlePotentialScore serverGame placed

handlePotentialScore :: ServerGame -> [(Pos, Tile)] -> IO ServerResponse
handlePotentialScore serverGame placedTiles =
  do
    gameState <- readTVarIO $ game serverGame
    let gameBoard = board gameState
    let formedWords =
          if moveNumber gameState > 1
            then wordsFormedMidGame gameBoard (M.fromList placedTiles)
            else wordFormedFirstMove gameBoard (M.fromList placedTiles)

    return $
      case formedWords of
        Left _ -> PotentialScore 0
        Right formed -> PotentialScore (overallScore formed)

{-
    If the move is successful, writes an update onto the broadcast channel and
    returns a server response to send to the moving client. Otherwise, returns
    an error to the client and writes nothing to the channel.
-}
handleMove ::
  ServerGame ->
  Pool SqlBackend ->
  Int ->
  Move ->
  (Either ScrabbleError GameTransition -> ServerResponse) ->
  IO ServerResponse
handleMove serverGame pool playerMoving move moveOutcomeHandler =
  do
    gameSnapshot <- atomically $ makeServerGameSnapshot serverGame
    let currentGameState = gameState gameSnapshot
    if playerNumber currentGameState /= playerMoving
      then return $ InvalidCommand "Not your move"
      else do
        let channel = broadcastChannel serverGame
        let moveOutcome = makeMove currentGameState move
        case moveOutcome of
          Left err -> return $ moveOutcomeHandler $ Left err
          Right transition -> do
            let eventMessage = transitionToMessage transition
            let newGameState = newGame transition

            P.persistGameUpdate pool (snapshotGameId gameSnapshot) newGameState eventMessage

            atomically $ do
              writeTVar (game serverGame) newGameState
              writeTChan channel eventMessage

            return $ moveOutcomeHandler moveOutcome

handleBoardMove :: ServerGame -> Pool SqlBackend -> Maybe Int -> [(Pos, Tile)] -> IO ServerResponse
handleBoardMove _ _ Nothing _ = return $ InvalidCommand "Observers cannot move"
handleBoardMove sharedServerGame pool (Just playerNo) placed =
  handleMove
    sharedServerGame
    pool
    playerNo
    (PlaceTiles $ M.fromList placed)
    moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand $ pack . show $ err
    moveOutcomeHandler (Right (MoveTransition newPlayer _ _)) = BoardMoveSuccess (tilesOnRack newPlayer)
    moveOutcomeHandler (Right _) = BoardMoveSuccess []

handleExchangeMove :: ServerGame -> Pool SqlBackend -> Maybe Int -> [Tile] -> IO ServerResponse
handleExchangeMove _ _ Nothing _ = return $ InvalidCommand "Observers cannot move"
handleExchangeMove sharedServerGame pool (Just playerNo) exchanged =
  handleMove
    sharedServerGame
    pool
    playerNo
    (Exchange exchanged)
    moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand $ pack . show $ err
    moveOutcomeHandler (Right (ExchangeTransition _ _ afterPlayer)) = ExchangeMoveSuccess (tilesOnRack afterPlayer)
    moveOutcomeHandler _ = InvalidCommand $ "internal server error, unexpected transition"

handlePassMove :: ServerGame -> Pool SqlBackend -> Maybe Int -> IO ServerResponse
handlePassMove _ _ Nothing = return $ InvalidCommand "Observers cannot move"
handlePassMove sharedServerGame pool (Just playerNo) =
  handleMove sharedServerGame pool playerNo Pass moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand $ pack . show $ err
    moveOutcomeHandler (Right _) = PassMoveSuccess

handleChatMessage :: ServerGame -> Pool SqlBackend -> Maybe AuthUser -> Text -> IO ServerResponse
handleChatMessage _ _ Nothing _ = return $ InvalidCommand "Observers cannot chat."
handleChatMessage serverGame pool (Just user) messageText =
  do
    gameSnapshot <- atomically $ makeServerGameSnapshot serverGame
    let serverPlayer = getServerPlayerSnapshot gameSnapshot user

    let playerName = serverPlayer >>= SP.name

    case playerName of
      Nothing -> return $ InvalidCommand "Internal server error"
      Just playerName -> do
        let channel = broadcastChannel serverGame
        let message = PlayerChat $ ChatMessage playerName messageText
        P.persistGameUpdate pool (gameId serverGame) (gameState gameSnapshot) message
        atomically $ writeTChan channel message
        return ChatSuccess
