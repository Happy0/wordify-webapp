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
import Controllers.Common.CacheableSharedResource (ResourceCache, makeGlobalResourceCache, getCacheableResource)
import Modules.Chats.Chat (getChat)
import Modules.Chats.Chatroom
import Repository.ChatRepository (ChatRepositoryImpl)
import UnliftIO.Resource (MonadResource)

data ChatService = ChatService
  { chatRooms :: ResourceCache Text Chatroom
  }

makeChatService :: ChatRepositoryImpl -> IO ChatService
makeChatService chatRepository = do
  rooms <- makeGlobalResourceCache (getChat chatRepository) (Just freezeChatroom)
  pure (ChatService rooms)

getChatroom :: (MonadResource m) => ChatService -> Text -> m (Either Text Chatroom)
getChatroom svc chatId = snd <$> getCacheableResource (chatRooms svc) chatId
