module Controllers.Chat.Model.Chatroom
  ( ChatMessage (ChatMessage),
    Chatroom,
    subscribeMessagesLive,
    makeChatroom,
    freezeChatroom,
    sendMessage,
    SendMessage (SendMessage),
  )
where

import ClassyPrelude (Bool (..), IO, Maybe (Nothing), Text, UTCTime, const, for_, forever, liftIO, pure, when, ($), (.), (<$>), (<*>), (>>))
import ClassyPrelude.Conduit (Monoid (mconcat))
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (STM, TChan, TVar, atomically, dupTChan, modifyTVar, newTChan, newTVarIO, readTChan, readTVar, writeTChan, writeTVar)
import qualified Data.Conduit as C (ConduitT, yield)
import Data.Time.Clock (getCurrentTime)
import GHC.Conc (killThread)
import Util.ConduitChan (chanSource)

-- TODO: include user IDs and do migration so that database 'chat' rows have user IDs
data SendMessage = SendMessage {userDisplayName :: Text, message :: Text}

data ChatMessage = ChatMessage {displayName :: Text, chatMessage :: Text, sentTime :: UTCTime}

data Chatroom = Chatroom
  { chatroomId :: Text,
    sequenceWriteChannel :: TChan SendMessage,
    chatBroadcastChannel :: TChan ChatMessage,
    persistChatMessage :: Text -> ChatMessage -> IO (),
    getChatMessages :: Text -> Maybe UTCTime -> C.ConduitT () ChatMessage IO (),
    thawed :: TVar Bool,
    threadId :: TVar (Maybe ThreadId)
  }

makeChatroom :: (Text -> ChatMessage -> IO ()) -> (Text -> Maybe UTCTime -> C.ConduitT () ChatMessage IO ()) -> Text -> IO Chatroom
makeChatroom persistChatMessage getChatMessagesLive chatroomId = do
  (writeChannel, broadcastChan) <- atomically $ (,) <$> newTChan <*> newTChan
  thawed <- newTVarIO False
  threadId <- newTVarIO Nothing
  pure (Chatroom chatroomId writeChannel broadcastChan persistChatMessage getChatMessagesLive thawed threadId)

sendMessage :: Chatroom -> SendMessage -> IO ()
sendMessage chatroom message = do
  thawChatroom chatroom
  atomically (writeTChan (sequenceWriteChannel chatroom) message)

subscribeMessagesLive :: Chatroom -> Maybe UTCTime -> C.ConduitT () ChatMessage IO ()
subscribeMessagesLive (Chatroom chatroomId _ subChannel _ getChatMessages _ _) since = do
  broadcastChannel <- liftIO $ (atomically . dupTChan) subChannel
  let existingChatMessages = getChatMessages chatroomId since
  let liveMessagesConduit = chanSource broadcastChannel
  mconcat [existingChatMessages, liveMessagesConduit]

thawChatroom :: Chatroom -> IO ()
thawChatroom chatroom = do
  chatroomNeedsThawed <- claimedAsGoingToThaw chatroom
  when chatroomNeedsThawed (startWorkerThread chatroom)

startWorkerThread :: Chatroom -> IO ()
startWorkerThread (Chatroom roomId sequenceWriteChannel broadcastChan persistChatMessage _ _ _) = forever $ do
  msg <- atomically (readTChan sequenceWriteChannel)
  now <- getCurrentTime
  let chatMessage = ChatMessage (userDisplayName msg) (message msg) now
  -- TODO: stop this loop from exiting due to IO errors
  persistChatMessage roomId chatMessage
  atomically (writeTChan broadcastChan chatMessage)

claimedAsGoingToThaw :: Chatroom -> IO Bool
claimedAsGoingToThaw chatroom =
  atomically $ do
    isAlreadyThawed <- readTVar (thawed chatroom)
    if isAlreadyThawed
      then pure False
      else modifyTVar (thawed chatroom) (const True) >> pure True

freezeChatroom :: Chatroom -> IO ()
freezeChatroom chatroom = do
  workerThreadId <- atomically (freezeChatroomTransaction chatroom)
  for_ workerThreadId killThread

freezeChatroomTransaction :: Chatroom -> STM (Maybe ThreadId)
freezeChatroomTransaction (Chatroom _ _ _ _ _ thawed threadId) = do
  writeTVar thawed False
  workerThreadId <- readTVar threadId
  writeTVar threadId Nothing
  pure workerThreadId