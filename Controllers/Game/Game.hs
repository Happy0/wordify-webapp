module Controllers.Game.Game(
    performRequest
) where

    import Prelude
    import Controllers.Game.Api
    import Controllers.Game.Model.ServerGame
    import qualified Controllers.Game.Persist as P
    import qualified Controllers.Game.Model.ServerPlayer as SP
    import Control.Monad
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.STM.TChan
    import Data.Conduit
    import qualified Data.Map as M
    import Data.Text
    import Wordify.Rules.FormedWord
    import Wordify.Rules.Game
    import Wordify.Rules.LetterBag
    import Wordify.Rules.Pos
    import Wordify.Rules.Player
    import Wordify.Rules.Move
    import Wordify.Rules.ScrabbleError
    import Wordify.Rules.Tile

    performRequest :: ServerGame -> Maybe Int -> ClientMessage -> IO ServerResponse
    performRequest serverGame player (BoardMove placed) = handleBoardMove serverGame player placed
    performRequest serverGame player (ExchangeMove exchanged) = handleExchangeMove serverGame player exchanged
    performRequest serverGame player PassMove = handlePassMove serverGame player
    performRequest serverGame player (SendChatMessage msg) = handleChatMessage serverGame player msg
    performRequest serverGame player (AskPotentialScore placed) = handlePotentialScore serverGame placed

    handlePotentialScore :: ServerGame -> [(Pos,Tile)] -> IO ServerResponse
    handlePotentialScore serverGame placedTiles =
        do
            gameState <- readTVarIO $ game serverGame
            let gameBoard = board gameState
            let formedWords = if (moveNumber gameState) > 1
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
    handleMove :: ServerGame ->
                Int ->
                Move ->
                (Either ScrabbleError GameTransition -> ServerResponse) ->
                IO ServerResponse
    handleMove serverGame playerMoving move moveOutcomeHandler=
        do
            gameState <- readTVarIO (game serverGame)
            if (playerNumber gameState) /= playerMoving
                then return $ InvalidCommand "Not your move"
                else do
                    let channel = broadcastChannel serverGame
                    let moveOutcome = makeMove gameState move
                    case moveOutcome of
                        Left err -> return $ moveOutcomeHandler $ Left err
                        Right transition -> do
                            atomically $ do
                                writeTChan channel (transitionToMessage transition)
                                writeTVar (game serverGame) $ newGame transition
                            return $ moveOutcomeHandler moveOutcome

    handleBoardMove :: ServerGame -> Maybe Int -> [(Pos, Tile)] -> IO ServerResponse
    handleBoardMove _ Nothing _ = return $ InvalidCommand "Observers cannot move"
    handleBoardMove sharedServerGame (Just playerNo) placed =
        handleMove
            sharedServerGame
            playerNo
            (PlaceTiles $ M.fromList (placed))
            moveOutcomeHandler
        where
            moveOutcomeHandler (Left err) = InvalidCommand $ pack . show $ err
            moveOutcomeHandler (Right (MoveTransition newPlayer _ _ )) = BoardMoveSuccess (tilesOnRack newPlayer)
            moveOutcomeHandler (Right _) = BoardMoveSuccess []

    handleExchangeMove :: ServerGame -> Maybe Int -> [Tile] -> IO ServerResponse
    handleExchangeMove _ Nothing _ = return $ InvalidCommand "Observers cannot move"
    handleExchangeMove sharedServerGame (Just playerNo) exchanged =
        handleMove
             sharedServerGame
             playerNo
             (Exchange exchanged)
             moveOutcomeHandler
        where
            moveOutcomeHandler (Left err) = InvalidCommand $ pack . show $ err
            moveOutcomeHandler (Right (ExchangeTransition _ beforePlayer afterPlayer)) = ExchangeMoveSuccess (tilesOnRack afterPlayer)
            moveOutcomeHandler _ = InvalidCommand $ "internal server error, unexpected transition"

    handlePassMove :: ServerGame ->  Maybe Int -> IO ServerResponse
    handlePassMove _ Nothing = return $ InvalidCommand "Observers cannot move"
    handlePassMove sharedServerGame (Just playerNo) = handleMove sharedServerGame playerNo Pass moveOutcomeHandler
        where
            moveOutcomeHandler (Left err) = InvalidCommand $ pack . show $ err
            moveOutcomeHandler (Right _) = PassMoveSuccess

    handleChatMessage :: ServerGame -> Maybe Int -> Text -> IO ServerResponse
    handleChatMessage _ Nothing _ = return $ InvalidCommand "Observers cannot chat."
    handleChatMessage serverGame (Just playerNumber) message =
        do
            let playerName = SP.name <$> (getServerPlayer serverGame playerNumber)

            case playerName of
                Nothing -> return $ InvalidCommand "Internal server error"
                Just name -> do
                    let channel = broadcastChannel serverGame
                    atomically $ writeTChan channel $ PlayerChat (ChatMessage name message)
                    return ChatSuccess
