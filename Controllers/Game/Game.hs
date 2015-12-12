module Controllers.Game.Game(
    handleChannelMessage,
    performRequest
) where

    import Prelude
    import Controllers.Game.Api
    import Controllers.Game.Model.ServerGame
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import qualified Data.Map as M
    import Data.Text
    import Wordify.Rules.Pos
    import Wordify.Rules.Move
    import Wordify.Rules.Tile

    handleChannelMessage :: GameMessage -> ServerResponse
    handleChannelMessage bleh = undefined

    performRequest :: TVar ServerGame -> ClientMessage -> IO ServerResponse
    performRequest serverGame (BoardMove placed) = handleBoardMove serverGame placed
    performRequest serverGame (ChatMessage msg) = error "Not implemented yet."

    handleBoardMove :: TVar ServerGame -> [(Pos, Tile)] -> IO ServerResponse
    handleBoardMove sharedServerGame placed =
        do
            serverGame <- readTVarIO sharedServerGame
            let gameState = game serverGame
            let move = PlaceTiles (M.fromList placed)
            let moveOutcome = makeMove gameState move

            case moveOutcome of
                Right (MoveTransition player newGame wordsFormed) ->
                    do
                        let updatedServerGame = serverGame {game = newGame}
                        atomically $ writeTVar sharedServerGame updatedServerGame
                        return BoardMoveSuccess

                Left err -> return $ InvalidCommand $ (pack . show) err
                    
            
