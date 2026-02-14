module Repository.ChatRepository
  ( toChatRepositoryImpl,
    ChatRepository (saveChatMessage, getChatMessages, countChatMessages),
    ChatRepositoryImpl,
    saveChatMessageImpl,
    getChatMessagesImpl,
    countChatMessagesImpl,
    ChatMessageEntity (ChatMessageEntity),
  )
where

import ClassyPrelude (IO, Int, Maybe)
import Conduit (ConduitT)
import qualified Data.Text as T
import Data.Time (UTCTime)

data ChatMessageEntity = ChatMessageEntity
  { chatroomId :: T.Text,
    senderUserId :: T.Text,
    senderDisplayName :: T.Text,
    message :: T.Text,
    timestamp :: UTCTime,
    messageNumber :: Int
  }

class ChatRepository a where
  saveChatMessage :: a -> ChatMessageEntity -> IO ()
  getChatMessages :: a -> T.Text -> Maybe Int -> ConduitT () ChatMessageEntity IO ()
  countChatMessages :: a -> T.Text -> IO Int

data ChatRepositoryImpl = ChatRepositoryImpl (ChatMessageEntity -> IO ()) (T.Text -> Maybe Int -> ConduitT () ChatMessageEntity IO ()) (T.Text -> IO Int)

toChatRepositoryImpl :: (ChatRepository a) => a -> ChatRepositoryImpl
toChatRepositoryImpl repository =
  ChatRepositoryImpl (saveChatMessage repository) (getChatMessages repository) (countChatMessages repository)

saveChatMessageImpl :: ChatRepositoryImpl -> ChatMessageEntity -> IO ()
saveChatMessageImpl (ChatRepositoryImpl saveChatMessageImpl _ _) = saveChatMessageImpl

getChatMessagesImpl :: ChatRepositoryImpl -> T.Text -> Maybe Int -> ConduitT () ChatMessageEntity IO ()
getChatMessagesImpl (ChatRepositoryImpl _ getChatMessagesImpl _) = getChatMessagesImpl

countChatMessagesImpl :: ChatRepositoryImpl -> T.Text -> IO Int
countChatMessagesImpl (ChatRepositoryImpl _ _ countChatMessagesImpl) = countChatMessagesImpl