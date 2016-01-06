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

    performRequest :: TVar ServerGame -> Maybe Int -> ClientMessage -> IO ServerResponse
    performRequest serverGame player (BoardMove placed) = handleBoardMove serverGame player placed
    performRequest serverGame player (ExchangeMove exchanged) = handleExchangeMove serverGame player exchanged
    performRequest serverGame player PassMove = handlePassMove serverGame player
    performRequest serverGame player (SendChatMessage msg) = handleChatMessage serverGame player msg
    performRequest serverGame player (AskPotentialScore placed) = handlePotentialScore serverGame placed

    handlePotentialScore :: TVar ServerGame -> [(Pos,Tile)] -> IO ServerResponse
    handlePotentialScore sharedServerGame placedTiles =
        do
            serverGame <- readTVarIO sharedServerGame
            let gameState = game serverGame
            let gameBoard = board gameState
            let formedWords = if (moveNumber gameState) > 1
                    then wordsFormedMidGame gameBoard (M.fromList placedTiles)
                        else wordFormedFirstMove gameBoard (M.fromList placedTiles)

            return $ case formedWords of
                Left _ -> PotentialScore 0
                Right formed -> PotentialScore (overallScore formed)

    {-
        If the move is successful, writes an update onto the broadcast channel and
        returns a server response to send to the moving client. Otherwise, returns
        an error to the client and writes nothing to the channel.
    -}
    handleMove :: TVar ServerGame ->
                Move ->
                (Either ScrabbleError GameTransition -> ServerResponse) ->
                IO ServerResponse
    handleMove sharedGame move moveOutcomeHandler=
        do
            serverGame <- readTVarIO sharedGame
            let channel = broadcastChannel serverGame
            let moveOutcome = makeMove (game serverGame) move
            case moveOutcome of
                Left err -> return $ moveOutcomeHandler $ Left err
                Right transition -> do
                    atomically $ do
                        writeTChan channel (transitionToMessage transition)
                        writeTVar sharedGame $ serverGame {game = newGame transition}
                    return $ moveOutcomeHandler moveOutcome

    handleBoardMove :: TVar ServerGame -> Maybe Int -> [(Pos, Tile)] -> IO ServerResponse
    handleBoardMove _ Nothing _ = return $ InvalidCommand "Observers cannot move"
    handleBoardMove sharedServerGame (Just playerNo) placed =
        do
            serverGame <- readTVarIO sharedServerGame
            let gameState = game serverGame

            if (playerNumber gameState) /= playerNo
                then return $ InvalidCommand "Not your move"
                else do
                    let moveOutcome = makeMove gameState (PlaceTiles (M.fromList placed))
                    let channel = broadcastChannel serverGame

                    case moveOutcome of
                        Right gameFinishedTransition@(GameFinished newGame maybeWords players ) -> do
                            atomically $ do
                                         writeTVar sharedServerGame serverGame { game = newGame}
                                         writeTChan channel $ GameEnd (transitionToSummary gameFinishedTransition)
                            return $ BoardMoveSuccess []

                        Right (MoveTransition newPlayerState newGame wordsFormed) ->
                            do
                                let moveSummary = toMoveSummary wordsFormed
                                let updatedServerGame = serverGame {game = newGame}
                                atomically $ do
                                     writeTChan channel $ (PlayerBoardMove (moveNumber newGame) placed moveSummary (players newGame) (playerNumber newGame) (bagSize (bag newGame)))
                                     writeTVar sharedServerGame updatedServerGame

                                return $ BoardMoveSuccess (tilesOnRack newPlayerState)

                        Left err -> return $ InvalidCommand $ (pack . show) err
                        _ -> return $ InvalidCommand "Internal server error. Expected board move"

    handleExchangeMove :: TVar ServerGame -> Maybe Int -> [Tile] -> IO ServerResponse
    handleExchangeMove _ Nothing _ = return $ InvalidCommand "Observers cannot move"
    handleExchangeMove sharedServerGame (Just playerNo) exchanged =
        do
            serverGame <- readTVarIO sharedServerGame
            let gameState = game serverGame

            if (playerNumber gameState) /= playerNo
                then return $ InvalidCommand "Not your move"
                else do
                    let moveOutcome = makeMove gameState (Exchange exchanged)
                    case moveOutcome of
                        Right et@(ExchangeTransition newGameState beforeExchangePlayer afterExchangePlayer) ->
                            do
                                let updatedServerGame = serverGame {game = newGameState}
                                let channel = broadcastChannel updatedServerGame
                                let summary = transitionToSummary et
                                atomically $ do
                                    writeTChan channel (PlayerExchangeMove (moveNumber newGameState) (playerNumber newGameState) exchanged summary)
                                    writeTVar sharedServerGame updatedServerGame

                                return $ ExchangeMoveSuccess (tilesOnRack afterExchangePlayer)

                        Left err -> return $ InvalidCommand $ (pack . show) err


    handlePassMove :: TVar ServerGame ->  Maybe Int -> IO ServerResponse
    handlePassMove _ Nothing = return $ InvalidCommand "Observers cannot move"
    handlePassMove sharedServerGame (Just playerNo) =
        do
            serverGame <- readTVarIO sharedServerGame
            let gameState = game serverGame
            let channel = broadcastChannel serverGame

            if (playerNumber gameState) /= playerNo
                then return $ InvalidCommand "Not your move"
                else do
                    let moveOutcome = makeMove gameState Pass
                    case moveOutcome of
                        Left err -> return $ InvalidCommand $ (pack . show) err
                        Right pt@(PassTransition newGame) ->
                            do
                                atomically $ do
                                    let summary = transitionToSummary pt
                                    writeTVar sharedServerGame (serverGame {game = newGame})
                                    writeTChan channel $ PlayerPassMove (moveNumber newGame) (playerNumber newGame) summary
                                return PassMoveSuccess
                        Right gameFinishedTransition@(GameFinished newGame maybeWords players ) ->
                                atomically $ do
                                    writeTVar sharedServerGame serverGame { game = newGame}
                                    writeTChan channel $ GameEnd (transitionToSummary gameFinishedTransition)
                                    return PassMoveSuccess

    handleChatMessage :: TVar ServerGame -> Maybe Int -> Text -> IO ServerResponse
    handleChatMessage _ Nothing _ = return $ InvalidCommand "Observers cannot chat."
    handleChatMessage sharedServerGame (Just playerNumber) message =
        do
            serverGame <- readTVarIO sharedServerGame
            let playerName = SP.name <$> (getServerPlayer serverGame playerNumber)

            case playerName of
                Nothing -> return $ InvalidCommand "Internal server error"
                Just name -> do
                    let channel = broadcastChannel serverGame
                    atomically $ writeTChan channel $ PlayerChat (ChatMessage name message)
                    return ChatSuccess
