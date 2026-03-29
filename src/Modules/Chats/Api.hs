module Modules.Chats.Api
  ( ChatService,
    makeChatService,
    getChatroom,
    Chatroom,
    subscribeMessagesLive,
    getExistingChatMessages,
    getMessagesSinceTime,
    sendMessage,
    SendMessage (SendMessage),
    ChatMessage (ChatMessage),
  )
where

import ClassyPrelude
import Data.SharedResourceCache (SharedResourceCache, makeGlobalSharedResourceCache, getCacheableResource, CacheExpiryConfig (..))
import Modules.Chats.Chat (getChat)
import Modules.Chats.Chatroom
import Repository.ChatRepository (ChatRepositoryImpl)
import UnliftIO.Resource (MonadResource)

data ChatService = ChatService
  { chatRooms :: SharedResourceCache Text Chatroom Text
  }

makeChatService :: ChatRepositoryImpl -> IO ChatService
makeChatService chatRepository = do
  rooms <- makeGlobalSharedResourceCache (getChat chatRepository) (Just freezeChatroom) (CacheExpiryConfig { sweepIntervalSeconds = 60, itemEligibleForRemovalAfterUnusedSeconds = 600 })
  pure (ChatService rooms)

getChatroom :: (MonadResource m) => ChatService -> Text -> m (Either Text Chatroom)
getChatroom svc chatId = snd <$> getCacheableResource (chatRooms svc) chatId
