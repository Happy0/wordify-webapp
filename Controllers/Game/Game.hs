module Controllers.Game.Game(
    performRequest
) where

    import Prelude
    import Controllers.Game.Api
    import Controllers.Game.Model.ServerGame
    import qualified Controllers.Game.Model.ServerPlayer as SP
    import Control.Monad
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.STM.TChan
    import qualified Data.Map as M
    import Data.Text
    import Wordify.Rules.Game
    import Wordify.Rules.LetterBag
    import Wordify.Rules.Pos
    import Wordify.Rules.Player
    import Wordify.Rules.Move
    import Wordify.Rules.Tile

    performRequest :: TVar ServerGame -> Maybe Int -> ClientMessage -> IO ServerResponse
    performRequest serverGame player (BoardMove placed) = handleBoardMove serverGame player placed
    performRequest serverGame player (ExchangeMove exchanged) = handleExchangeMove serverGame player exchanged
    performRequest serverGame player PassMove = handlePassMove serverGame player
    performRequest serverGame player (ChatMessage msg) = handleChatMessage serverGame player msg

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

                    case moveOutcome of
                        Right (GameFinished game maybeWords players ) ->
                            return $ InvalidCommand "game finish not handled yet."

                        Right (MoveTransition newPlayerState newGame wordsFormed) ->
                            do
                                let moveSummary = toMoveSummary wordsFormed
                                let newSummaries = moveSummaries serverGame ++ [moveSummary]
                                let updatedServerGame = serverGame {game = newGame, moveSummaries = newSummaries}
                                let channel = broadcastChannel updatedServerGame
                                atomically $ do
                                     writeTChan channel (PlayerBoardMove placed moveSummary (players newGame) (playerNumber newGame) (bagSize (bag newGame)))
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
                        Right (ExchangeTransition newGameState beforeExchangePlayer afterExchangePlayer) ->
                            do
                                let summary = ExchangeMoveSummary 
                                let newSummaries = (moveSummaries serverGame ++ [summary])
                                let updatedServerGame = serverGame {game = newGameState, moveSummaries = newSummaries}
                                let channel = broadcastChannel updatedServerGame
                                atomically $ do
                                    writeTChan channel (PlayerExchangeMove (playerNumber newGameState) summary)
                                    writeTVar sharedServerGame updatedServerGame

                                return $ ExchangeMoveSuccess (tilesOnRack afterExchangePlayer)

                        Left err -> return $ InvalidCommand $ (pack . show) err


    handlePassMove :: TVar ServerGame ->  Maybe Int -> IO ServerResponse
    handlePassMove _ Nothing = return $ InvalidCommand "Observers cannot move"
    handlePassMove sharedServerGame (Just playerNo) =
        do
            serverGame <- readTVarIO sharedServerGame
            let gameState = game serverGame

            if (playerNumber gameState) /= playerNo
                then return $ InvalidCommand "Not your move"
                else do
                    let moveOutcome = makeMove gameState Pass
                    case moveOutcome of
                        Left err -> return $ InvalidCommand $ (pack . show) err
                        Right (PassTransition newGame) ->
                            do
                                let channel = broadcastChannel serverGame
                                let summary = PassMoveSummary
                                let newSummaries = (moveSummaries serverGame ++ [summary])
                                atomically $ do
                                    writeTVar sharedServerGame (serverGame {game = newGame, moveSummaries = newSummaries})
                                    writeTChan channel $ PlayerPassMove (playerNumber newGame) summary
                                return PassMoveSuccess
                        Right (GameFinished game maybeWords players ) ->
                            return $ InvalidCommand "game finish not handled yet."

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
                    atomically $ writeTChan channel $ (PlayerChat name message)
                    return ChatSuccess
