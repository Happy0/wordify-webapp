module Repository.SQL.SqlChatRepository (SqlChatRepositoryBackend (SqlChatRepositoryBackend)) where

import ClassyPrelude (Bool (False, True), IO, Int, Maybe (Just, Nothing), Monad, MonadIO, UTCTime, flip, lift, liftIO, return, undefined, ($), (>))
import Conduit (runConduit)
import qualified Conduit as C (ConduitT, lengthC)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.List as CL (filter, sourceList)
import qualified Data.List as L
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist (insert)
import Database.Persist.Sql
import qualified Model as M
import Repository.ChatRepository (ChatMessageEntity (ChatMessageEntity), ChatRepository (countChatMessages, getChatMessages, saveChatMessage))

newtype SqlChatRepositoryBackend = SqlChatRepositoryBackend (Pool SqlBackend)

instance ChatRepository SqlChatRepositoryBackend where
  saveChatMessage (SqlChatRepositoryBackend pool) = saveChatMessageImpl pool
  getChatMessages (SqlChatRepositoryBackend pool) = getChatMessagesImpl pool

  countChatMessages (SqlChatRepositoryBackend pool) = countChatMessagesImpl pool

saveChatMessageImpl :: Pool SqlBackend -> ChatMessageEntity -> IO ()
saveChatMessageImpl pool (ChatMessageEntity chatroomId senderDisplayName message now _) = do
  withPool pool $ do
    _ <- insert (M.ChatMessage chatroomId now senderDisplayName message)
    return ()

-- TODO: do this in SQL
countChatMessagesImpl :: Pool SqlBackend -> T.Text -> IO Int
countChatMessagesImpl pool chatroomId = runConduit $ getChatMessagesImpl pool chatroomId Nothing .| C.lengthC

-- TODO: this loads all messages into memory rather than truely streaming them. Even the Persist SQL's 'selectSource' implementation
-- doesn't truely stream - would need ot manually page through the results. in a custom 'stream source' implementation
getChatMessagesImpl :: (MonadIO m) => Pool SqlBackend -> T.Text -> Maybe Int -> C.ConduitT () ChatMessageEntity m ()
getChatMessagesImpl pool chatroomId sinceMessageNumber = do
  -- TODO: rename ChatMessageGame field to ChatMessageRoomId
  allMessages <- liftIO (withPool pool $ selectList [M.ChatMessageGame ==. chatroomId] [])
  let messages = L.zipWith chatMessageFromEntity [1 ..] allMessages
  -- TODO: store the message number in the database and use that to filter in the query - needs migration
  CL.sourceList messages .| CL.filter (isGreaterThanMessageNumber sinceMessageNumber)
  where
    isGreaterThanMessageNumber :: Maybe Int -> ChatMessageEntity -> Bool
    isGreaterThanMessageNumber Nothing _ = True
    isGreaterThanMessageNumber (Just since) (ChatMessageEntity _ _ _ _ messageNumber) = messageNumber > since

chatMessageFromEntity :: Int -> Entity M.ChatMessage -> ChatMessageEntity
chatMessageFromEntity messageNumber (Entity _ (M.ChatMessage roomId created user message)) = ChatMessageEntity roomId user message created messageNumber

withPool = flip runSqlPersistMPool
