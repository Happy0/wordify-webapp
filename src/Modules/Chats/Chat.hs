module Modules.Chats.Chat (getChat) where

import ClassyPrelude
import Modules.Chats.Chatroom (ChatMessage (ChatMessage), Chatroom, makeChatroom)
import qualified Data.Conduit as C (ConduitT, (.|))
import qualified Data.Conduit.List as CL (map)
import qualified Data.Text as T
import Repository.ChatRepository (ChatMessageEntity (ChatMessageEntity), ChatRepositoryImpl, countChatMessagesImpl, getChatMessagesImpl, saveChatMessageImpl)

getChat :: ChatRepositoryImpl -> T.Text -> IO (Either Text Chatroom)
getChat _ "" = pure (Left "Chatroom ID cannot be empty")
getChat chatRepository chatroomId = Right <$> makeChatroom (saveChatMessage chatRepository) (getChatMessages chatRepository) (countChatMessagesImpl chatRepository) chatroomId

saveChatMessage :: ChatRepositoryImpl -> Text -> ChatMessage -> IO ()
saveChatMessage chatRepository chatroomId msg = saveChatMessageImpl chatRepository (toChatMessageEntity chatroomId msg)

getChatMessages :: ChatRepositoryImpl -> Text -> Maybe Int -> C.ConduitT () ChatMessage IO ()
getChatMessages chatRepository roomId since = getChatMessagesImpl chatRepository roomId since C..| CL.map fromChatMessageEntity

fromChatMessageEntity :: ChatMessageEntity -> ChatMessage
fromChatMessageEntity (ChatMessageEntity _ senderUserId senderDisplayName message now chatMessageNumber) =
  ChatMessage senderUserId senderDisplayName message now chatMessageNumber

toChatMessageEntity :: Text -> ChatMessage -> ChatMessageEntity
toChatMessageEntity roomId (ChatMessage senderUserId senderDisplayName message now chatMessageNumber) =
  ChatMessageEntity roomId senderUserId senderDisplayName message now chatMessageNumber
