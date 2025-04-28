module Controllers.Chat.Chatroom
  ( ChatMessage (ChatMessage),
    Chatroom,
    subscribeMessagesLive,
    makeChatroom,
    freezeChatroom,
    sendMessage,
    SendMessage (SendMessage),
  )
where

import ClassyPrelude (IO, Int, Maybe (Nothing), Monoid (mconcat), Text, UTCTime, forever, liftIO, pure, ($), (+), (.), (<$>), (<*>))
import Control.Concurrent.STM (TChan, atomically, dupTChan, newTChan, readTChan, writeTChan)
import qualified Data.Conduit as C (ConduitT)
import Data.Time.Clock (getCurrentTime)
import Util.ConduitChan (chanSource)
import Util.WorkerThread (WorkerThread, newUnstartedWorkerThread, startIfNotStarted, stopIfNotStopped)

-- TODO: include user IDs and do migration so that database 'chat' rows have user IDs
data SendMessage = SendMessage {userDisplayName :: Text, message :: Text}

data ChatMessage = ChatMessage {displayName :: Text, chatMessage :: Text, sentTime :: UTCTime, messageNumber :: Int}

data Chatroom = Chatroom
  { chatroomId :: Text,
    sequenceWriteChannel :: TChan SendMessage,
    chatBroadcastChannel :: TChan ChatMessage,
    persistChatMessage :: Text -> ChatMessage -> IO (),
    getChatMessages :: Text -> Maybe Int -> C.ConduitT () ChatMessage IO (),
    countChatMessages :: Text -> IO Int,
    workerThread :: WorkerThread
  }

makeChatroom :: (Text -> ChatMessage -> IO ()) -> (Text -> Maybe Int -> C.ConduitT () ChatMessage IO ()) -> (Text -> IO Int) -> Text -> IO Chatroom
makeChatroom persistChatMessage getChatMessagesLive countChatMessages chatroomId = do
  (writeChannel, broadcastChan) <- atomically $ (,) <$> newTChan <*> newTChan
  workerThread <- atomically newUnstartedWorkerThread
  let chatroom = Chatroom chatroomId writeChannel broadcastChan persistChatMessage getChatMessagesLive countChatMessages workerThread
  pure chatroom

sendMessage :: Chatroom -> SendMessage -> IO ()
sendMessage chatroom message = do
  thawChatroom chatroom
  atomically (writeTChan (sequenceWriteChannel chatroom) message)

subscribeMessagesLive :: Chatroom -> Maybe Int -> C.ConduitT () ChatMessage IO ()
subscribeMessagesLive (Chatroom chatroomId _ subChannel _ getChatMessages _ _) sinceMessageNumber = do
  broadcastChannel <- liftIO $ (atomically . dupTChan) subChannel
  let existingChatMessages = getChatMessages chatroomId sinceMessageNumber
  let liveMessagesConduit = chanSource broadcastChannel
  mconcat [existingChatMessages, liveMessagesConduit]

workerLoop :: Chatroom -> IO loop
workerLoop (Chatroom roomId sequenceWriteChannel broadcastChan persistChatMessage _ countChatMessages _) = forever $ do
  msg <- atomically (readTChan sequenceWriteChannel)
  now <- getCurrentTime
  messageNumber <- (+ 1) <$> countChatMessages roomId
  let chatMessage = ChatMessage (userDisplayName msg) (message msg) now messageNumber
  -- TODO: stop this loop from exiting due to IO errors
  persistChatMessage roomId chatMessage
  atomically (writeTChan broadcastChan chatMessage)

thawChatroom :: Chatroom -> IO ()
thawChatroom chatroom = startIfNotStarted (workerThread chatroom) (workerLoop chatroom)

freezeChatroom :: Chatroom -> IO ()
freezeChatroom chatroom = stopIfNotStopped (workerThread chatroom)