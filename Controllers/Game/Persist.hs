module Controllers.Game.Persist (getChatMessages, persistNewGame) where

    import Prelude
    import Control.Concurrent
    import Control.Monad
    import Control.Monad.IO.Class
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TChan
    import Controllers.Game.Api
    import Controllers.Game.Model.ServerGame
    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import Data.Pool
    import Database.Persist.Sql
    import Data.Text
    import Data.Time.Clock
    import qualified Model as M

    chatMessageFromEntity :: Entity M.ChatMessage -> GameMessage
    chatMessageFromEntity (Entity _ (M.ChatMessage _ created user message )) = PlayerChat (ChatMessage user message)

    getChatMessages pool gameId sink = liftIO $ withPool pool $ 
                 return $ selectSource [M.ChatMessageGame ==. gameId] [Asc M.ChatMessageCreatedAt]
                    $= CL.map chatMessageFromEntity
                    $$ sink

    {-
        Persists the original game state (before the game has begun) and
        then listens for game events and updates the game in storage as
        it is played
    -}
    persistNewGame :: Pool SqlBackend -> Text -> ServerGame -> TChan GameMessage -> IO ()
    persistNewGame pool gameId serverGame messageChannel = do
        forkIO $ do
             key <- persistGameState pool gameId serverGame
             watchForUpdates pool gameId  messageChannel
             return ()

        return ()

    withPool pool = flip runSqlPersistMPool pool

    persistGameState :: Pool SqlBackend -> Text -> ServerGame -> IO (Key M.Game)
    persistGameState pool gameId serverGame =
         withPool pool $ do
            insert $ M.Game gameId

    watchForUpdates :: Pool SqlBackend -> Text -> TChan GameMessage -> IO ()
    watchForUpdates pool gameId messageChannel =
        forever $ (atomically . readTChan) messageChannel >>= persistUpdate pool gameId

    persistUpdate :: Pool SqlBackend -> Text -> GameMessage -> IO ()
    persistUpdate pool gameId (PlayerChat (ChatMessage user message)) = do
        time <- getCurrentTime
        withPool pool $ do
            insert $ M.ChatMessage gameId time user message
            return ()

