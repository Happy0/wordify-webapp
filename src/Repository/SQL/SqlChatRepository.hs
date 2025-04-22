module Repository.SQL.SqlChatRepository (SqlChatRepositoryBackend (SqlChatRepositoryBackend)) where

import ClassyPrelude (IO, Maybe (Just, Nothing), Monad, MonadIO, UTCTime, flip, lift, liftIO, return, undefined, ($))
import Conduit (ConduitT)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.List as L
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

-- TODO: this loads all messages into memory rather than truely streaming them. Even the Persist SQL's 'selectSource' implementation
-- doesn't truely stream - would need ot manually page through the results. in a custom 'stream source' implementation
getChatMessagesImpl :: (MonadIO m) => Pool SqlBackend -> T.Text -> Maybe UTCTime -> ConduitT () ChatMessageEntity m ()
getChatMessagesImpl pool chatroomId (Just messagesAfter) = do
  -- TODO: rename ChatMessageGame field to ChatMessageRoomId
  allMessages <- liftIO (withPool pool $ selectList [M.ChatMessageGame ==. chatroomId, M.ChatMessageCreatedAt >. messagesAfter] [])
  let messages = L.map chatMessageFromEntity allMessages
  CL.sourceList messages
getChatMessagesImpl pool chatroomId Nothing = do
  allMessages <- liftIO (withPool pool $ selectList [M.ChatMessageGame ==. chatroomId] [])
  let messages = L.map chatMessageFromEntity allMessages
  CL.sourceList messages

chatMessageFromEntity :: Entity M.ChatMessage -> ChatMessageEntity
chatMessageFromEntity (Entity _ (M.ChatMessage roomId created user message)) = ChatMessageEntity roomId user message created

withPool = flip runSqlPersistMPool
