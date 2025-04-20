module Repository.SQL.SqlChatRepository (SqlChatRepositoryBackend (SqlChatRepositoryBackend)) where

import ClassyPrelude (IO, Maybe, UTCTime, undefined)
import Conduit (ConduitT)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Sql
import Repository.ChatRepository (ChatMessage, ChatRepository (getChatMessages, saveChatMessage))

data SqlChatRepositoryBackend = SqlChatRepositoryBackend (Pool SqlBackend)

instance ChatRepository SqlChatRepositoryBackend where
  saveChatMessage (SqlChatRepositoryBackend pool) = saveChatMessageImpl pool
  getChatMessages (SqlChatRepositoryBackend pool) = getChatMessagesImpl pool

saveChatMessageImpl :: Pool SqlBackend -> ChatMessage -> IO ()
saveChatMessageImpl pool chatMessage = undefined

getChatMessagesImpl :: Pool SqlBackend -> T.Text -> Maybe UTCTime -> ConduitT () ChatMessage IO ()
getChatMessagesImpl pool chatroomId messagesAfter = undefined
