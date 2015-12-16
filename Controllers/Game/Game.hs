module Controllers.Game.Game(
    performRequest
) where

    import Prelude
    import Controllers.Game.Api
    import Controllers.Game.Model.ServerGame
    import Control.Monad
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.STM.TChan
    import qualified Data.Map as M
    import Data.Text
    import Wordify.Rules.Game
    import Wordify.Rules.Pos
    import Wordify.Rules.Player
    import Wordify.Rules.Move
    import Wordify.Rules.Tile

    performRequest :: TVar ServerGame -> Maybe Int -> ClientMessage -> IO ServerResponse
    performRequest serverGame player (BoardMove placed) = handleBoardMove serverGame player placed
    performRequest serverGame player PassMove = handlePassMove serverGame player
    performRequest serverGame player (ChatMessage msg) = error "Not implemented yet."

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
                                let updatedServerGame = serverGame {game = newGame}
                                let channel = broadcastChannel updatedServerGame
                                atomically $ do
                                     writeTChan channel (PlayerBoardMove placed (players newGame) (playerNumber newGame))
                                     writeTVar sharedServerGame updatedServerGame

                                return $ BoardMoveSuccess (tilesOnRack newPlayerState)

                        Left err -> return $ InvalidCommand $ (pack . show) err
                        _ -> return $ InvalidCommand "Internal server error. Expected board move"
    
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
                                atomically $ do
                                    writeTVar sharedServerGame (serverGame {game = newGame})
                                    writeTChan channel $ PlayerPassMove (playerNumber newGame)
                                return PassMoveSuccess
                        Right (GameFinished game maybeWords players ) -> 
                            return $ InvalidCommand "game finish not handled yet."


