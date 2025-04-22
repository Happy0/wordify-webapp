module Controllers.Chat (getChat) where

import ClassyPrelude
import Controllers.Chat.Model.Chatroom (ChatMessage (ChatMessage), Chatroom, makeChatroom)
import qualified Data.Conduit as C (ConduitT, (.|))
import qualified Data.Conduit.List as CL (map)
import qualified Data.Text as T
import Repository.ChatRepository (ChatMessageEntity (ChatMessageEntity), ChatRepositoryImpl, getChatMessagesImpl, saveChatMessageImpl)

getChat :: ChatRepositoryImpl -> T.Text -> IO Chatroom
getChat chatRepository = makeChatroom (saveChatMessage chatRepository) (getChatMessages chatRepository)

saveChatMessage :: ChatRepositoryImpl -> Text -> ChatMessage -> IO ()
saveChatMessage chatRepository chatroomId msg = saveChatMessageImpl chatRepository (toChatMessageEntity chatroomId msg)

getChatMessages :: ChatRepositoryImpl -> Text -> Maybe UTCTime -> C.ConduitT () ChatMessage IO ()
getChatMessages chatRepository roomId since = getChatMessagesImpl chatRepository roomId since C..| CL.map fromChatMessageEntity

fromChatMessageEntity :: ChatMessageEntity -> ChatMessage
fromChatMessageEntity (ChatMessageEntity _ senderDisplayName message now) =
  ChatMessage senderDisplayName message now

toChatMessageEntity :: Text -> ChatMessage -> ChatMessageEntity
toChatMessageEntity roomId (ChatMessage senderDisplayName message now) =
  ChatMessageEntity roomId senderDisplayName message now
