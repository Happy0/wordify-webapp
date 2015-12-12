module Controllers.Game.Game(
    handleChannelMessage,
    performRequest
) where

    import Prelude
    import Controllers.Game.Api
    import Controllers.Game.Model.ServerGame
    import Control.Concurrent.STM.TVar

    handleChannelMessage :: GameMessage -> ServerResponse
    handleChannelMessage bleh = undefined

    performRequest :: TVar ServerGame -> ClientMessage -> IO ServerResponse
    performRequest serverGame (BoardMove placed) = return BoardMoveSuccess
    performRequest serverGame (ChatMessage msg) = error "Not implemented yet."
