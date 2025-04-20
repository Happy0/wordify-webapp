module Repository.SQL.SqlChatRepository (SqlChatRepositoryBackend (SqlChatRepositoryBackend)) where

import ClassyPrelude (IO, Maybe (Just, Nothing), Monad, MonadIO, UTCTime, flip, liftIO, return, undefined, ($))
import Conduit (ConduitT)
import Data.Conduit (Conduit, (.|))
import qualified Data.Conduit.List as CL
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist (insert)
import Database.Persist.Sql
import qualified Model as M
import Repository.ChatRepository (ChatMessageEntity (ChatMessageEntity), ChatRepository (getChatMessages, saveChatMessage))

data SqlChatRepositoryBackend = SqlChatRepositoryBackend (Pool SqlBackend)

instance ChatRepository SqlChatRepositoryBackend where
  saveChatMessage (SqlChatRepositoryBackend pool) = saveChatMessageImpl pool
  getChatMessages (SqlChatRepositoryBackend pool) = getChatMessagesImpl pool

saveChatMessageImpl :: Pool SqlBackend -> ChatMessageEntity -> IO ()
saveChatMessageImpl pool (ChatMessageEntity chatroomId senderDisplayName message now) = do
  withPool pool $ do
    _ <- insert (M.ChatMessage chatroomId now senderDisplayName message)
    return ()

getChatMessagesImpl :: Pool SqlBackend -> T.Text -> Maybe UTCTime -> ConduitT () ChatMessageEntity IO ()
getChatMessagesImpl pool chatroomId (Just messagesAfter) = undefined

chatMessageFromEntity :: (Monad m) => Conduit (Entity M.ChatMessage) m ChatMessageEntity
chatMessageFromEntity = CL.map fromEntity
  where
    fromEntity (Entity _ (M.ChatMessage roomId created user message)) = ChatMessageEntity roomId user message created

withPool pool = flip runSqlPersistMPool pool