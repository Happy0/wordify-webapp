module Repository.ChatRepository
  ( toChatRepositoryImpl,
    ChatRepository (saveChatMessage, getChatMessages),
    ChatRepositoryImpl,
    saveChatMessageImpl,
    getChatMessagesImpl,
    ChatMessageEntity (ChatMessageEntity),
  )
where

import ClassyPrelude (IO, Int, Maybe)
import Conduit (ConduitT)
import qualified Data.Text as T
import Data.Time (UTCTime)

data ChatMessageEntity = ChatMessageEntity
  { -- TODO: this should be a userID but needs a DB migration
    chatroomId :: T.Text,
    senderDisplayName :: T.Text,
    message :: T.Text,
    timestamp :: UTCTime,
    messageNumber :: Int
  }

class ChatRepository a where
  saveChatMessage :: a -> ChatMessageEntity -> IO ()
  getChatMessages :: a -> T.Text -> Maybe Int -> ConduitT () ChatMessageEntity IO ()

data ChatRepositoryImpl = ChatRepositoryImpl (ChatMessageEntity -> IO ()) (T.Text -> Maybe Int -> ConduitT () ChatMessageEntity IO ())

toChatRepositoryImpl :: (ChatRepository a) => a -> ChatRepositoryImpl
toChatRepositoryImpl repository =
  ChatRepositoryImpl (saveChatMessage repository) (getChatMessages repository)

saveChatMessageImpl :: ChatRepositoryImpl -> ChatMessageEntity -> IO ()
saveChatMessageImpl (ChatRepositoryImpl saveChatMessageImpl _) = saveChatMessageImpl

getChatMessagesImpl :: ChatRepositoryImpl -> T.Text -> Maybe Int -> ConduitT () ChatMessageEntity IO ()
getChatMessagesImpl (ChatRepositoryImpl _ getChatMessagesImpl) = getChatMessagesImpl