module Controllers.Game.Game
  ( performRequest,
  )
where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Controllers.Game.Api
import Controllers.Game.Model.ServerGame
import qualified Controllers.Game.Model.ServerPlayer as SP
import qualified Controllers.Game.Persist as P
import Data.Conduit
import qualified Data.Map as M
import Data.Pool
import Data.Text
import Database.Persist.Sql
import Wordify.Rules.FormedWord
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import Wordify.Rules.Player
import Wordify.Rules.Pos
import Wordify.Rules.ScrabbleError
import Wordify.Rules.Tile
import Prelude

performRequest :: ServerGame -> Pool SqlBackend -> Maybe Int -> ClientMessage -> IO ServerResponse
performRequest serverGame pool player (BoardMove placed) = handleBoardMove serverGame pool player placed
performRequest serverGame pool player (ExchangeMove exchanged) = handleExchangeMove serverGame pool player exchanged
performRequest serverGame pool player PassMove = handlePassMove serverGame pool player
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
    moveOutcomeHandler (Right (ExchangeTransition _ beforePlayer afterPlayer)) = ExchangeMoveSuccess (tilesOnRack afterPlayer)
    moveOutcomeHandler _ = InvalidCommand $ "internal server error, unexpected transition"

handlePassMove :: ServerGame -> Pool SqlBackend -> Maybe Int -> IO ServerResponse
handlePassMove _ _ Nothing = return $ InvalidCommand "Observers cannot move"
handlePassMove sharedServerGame pool (Just playerNo) =
  handleMove sharedServerGame pool playerNo Pass moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand $ pack . show $ err
    moveOutcomeHandler (Right _) = PassMoveSuccess

handleChatMessage :: ServerGame -> Pool SqlBackend -> Maybe Int -> Text -> IO ServerResponse
handleChatMessage _ _ Nothing _ = return $ InvalidCommand "Observers cannot chat."
handleChatMessage serverGame pool (Just playerNumber) messageText =
  do
    (serverPlayer, gameSnapshot) <- atomically $ (,) <$> getServerPlayer serverGame playerNumber <*> makeServerGameSnapshot serverGame
    let playerName = serverPlayer >>= SP.name

    case playerName of
      Nothing -> return $ InvalidCommand "Internal server error"
      Just playerName -> do
        let channel = broadcastChannel serverGame
        let message = PlayerChat $ ChatMessage playerName messageText
        P.persistGameUpdate pool (gameId serverGame) (gameState gameSnapshot) message
        atomically $ writeTChan channel message
        return ChatSuccess
