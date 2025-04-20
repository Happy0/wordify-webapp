import ClassyPrelude (IO, Maybe)
import Conduit (ConduitT)
import qualified Data.Text as T
import Data.Time (UTCTime)

data ChatMessage = ChatMessage
  { -- TODO: this should be a userID but needs a DB migration
    senderDisplayName :: T.Text,
    message :: T.Text,
    timestamp :: UTCTime
  }

class ChatRepository a where
  saveChatMessage :: a -> ChatMessage -> IO ()
  getChatMessages :: a -> T.Text -> Maybe UTCTime -> ConduitT () ChatMessage IO ()

data ChatRepositoryImpl = ChatRepositoryImpl (ChatMessage -> IO ()) (T.Text -> Maybe UTCTime -> ConduitT () ChatMessage IO ())

toChatRepositoryImpl :: (ChatRepository a) => a -> ChatRepositoryImpl
toChatRepositoryImpl repository =
  ChatRepositoryImpl (saveChatMessage repository) (getChatMessages repository)

saveChatMessageImpl :: ChatRepositoryImpl -> ChatMessage -> IO ()
saveChatMessageImpl (ChatRepositoryImpl saveChatMessageImpl _) = saveChatMessageImpl

getChatMessagesImpl :: ChatRepositoryImpl -> T.Text -> Maybe UTCTime -> ConduitT () ChatMessage IO ()
getChatMessagesImpl (ChatRepositoryImpl _ getChatMessagesImpl) = getChatMessagesImpl