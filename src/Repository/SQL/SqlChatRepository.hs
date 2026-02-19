module Repository.SQL.SqlChatRepository (SqlChatRepositoryBackend (SqlChatRepositoryBackend)) where

import ClassyPrelude (Bool (False, True), IO, Int, Maybe (Just, Nothing), Monad, MonadIO, UTCTime, flip, fromMaybe, lift, liftIO, return, undefined, ($), (>))
import Conduit (runConduit)
import qualified Conduit as C (ConduitT, lengthC)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.List as CL (filter, sourceList)
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist (insert, getMany, selectList, (==.), SelectOpt(Asc))
import Database.Persist.Sql
import qualified Model as M
import Repository.ChatRepository (ChatMessageEntity (ChatMessageEntity), ChatRepository (countChatMessages, getChatMessages, saveChatMessage))

newtype SqlChatRepositoryBackend = SqlChatRepositoryBackend (Pool SqlBackend)

instance ChatRepository SqlChatRepositoryBackend where
  saveChatMessage (SqlChatRepositoryBackend pool) = saveChatMessageImpl pool
  getChatMessages (SqlChatRepositoryBackend pool) = getChatMessagesImpl pool

  countChatMessages (SqlChatRepositoryBackend pool) = countChatMessagesImpl pool

saveChatMessageImpl :: Pool SqlBackend -> ChatMessageEntity -> IO ()
saveChatMessageImpl pool (ChatMessageEntity chatroomId senderUserId _ msg now _) = do
  withPool pool $ do
    _ <- insert (M.ChatMessage chatroomId now (M.UserKey senderUserId) msg)
    return ()

-- TODO: do this in SQL
countChatMessagesImpl :: Pool SqlBackend -> T.Text -> IO Int
countChatMessagesImpl pool chatroomId = runConduit $ getChatMessagesImpl pool chatroomId Nothing .| C.lengthC

-- TODO: this loads all messages into memory rather than truely streaming them. Even the Persist SQL's 'selectSource' implementation
-- doesn't truely stream - would need ot manually page through the results. in a custom 'stream source' implementation
getChatMessagesImpl :: (MonadIO m) => Pool SqlBackend -> T.Text -> Maybe Int -> C.ConduitT () ChatMessageEntity m ()
getChatMessagesImpl pool chatroomId sinceMessageNumber = do
  (allMessages, userMap) <- liftIO $ withPool pool $ do
    msgs <- selectList [M.ChatMessageChatId ==. chatroomId] [Asc M.ChatMessageCreatedAt]
    let userIds = L.nub [sentBy | Entity _ (M.ChatMessage _ _ sentBy _) <- msgs]
    users <- getMany userIds
    return (msgs, users)
  let messages = L.zipWith (chatMessageFromEntity userMap) [1 ..] allMessages
  -- TODO: store the message number in the database and use that to filter in the query - needs migration
  CL.sourceList messages .| CL.filter (isGreaterThanMessageNumber sinceMessageNumber)
  where
    isGreaterThanMessageNumber :: Maybe Int -> ChatMessageEntity -> Bool
    isGreaterThanMessageNumber Nothing _ = True
    isGreaterThanMessageNumber (Just since) (ChatMessageEntity _ _ _ _ _ messageNumber) = messageNumber > since

chatMessageFromEntity :: Map.Map M.UserId M.User -> Int -> Entity M.ChatMessage -> ChatMessageEntity
chatMessageFromEntity userMap messageNumber (Entity _ (M.ChatMessage roomId created userId@(M.UserKey userIdText) message)) =
  let displayName = case Map.lookup userId userMap of
        Just user -> fromMaybe (fromMaybe userIdText (M.userNickname user)) (M.userUsername user)
        Nothing -> "Unknown"
  in ChatMessageEntity roomId userIdText displayName message created messageNumber

withPool = flip runSqlPersistMPool
