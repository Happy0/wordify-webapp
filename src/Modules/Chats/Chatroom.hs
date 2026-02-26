module Modules.Chats.Chatroom
  ( ChatMessage (ChatMessage),
    Chatroom,
    subscribeMessagesLive,
    getExistingChatMessages,
    makeChatroom,
    freezeChatroom,
    sendMessage,
    SendMessage (SendMessage)
  )
where

import ClassyPrelude (Exception (fromException), IO, Int, Maybe (Nothing), Monoid (mconcat), Text, UTCTime, const, forever, isJust, isNothing, liftIO, pack, pure, putStrLn, show, unless, ($), (+), (.), (<$>), (<*>))
import ClassyPrelude.Conduit (Maybe (Just))
import Control.Concurrent.STM (TChan, atomically, dupTChan, newTChan, readTChan, writeTChan)
import Control.Error (maybe)
import Control.Exception (SomeException, throw, try)
import Control.Exception.Base (AsyncException)
import Control.Monad (Monad ((>>)), when)
import Data.Bool (Bool, not)
import qualified Data.Conduit as C (ConduitT)
import Data.Either (Either (Left, Right))
import Data.Time.Clock (getCurrentTime)
import Util.ConduitChan (chanSource)
import Util.Exception (printAndIgnoreSyncException)
import Util.WorkerThread (WorkerThread, newUnstartedWorkerThread, startIfNotStarted, stopIfNotStopped)

data SendMessage = SendMessage {senderUserId :: Text, userDisplayName :: Text, message :: Text}

data ChatMessage = ChatMessage {chatSenderUserId :: Text, displayName :: Text, chatMessage :: Text, sentTime :: UTCTime, messageNumber :: Int}

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

getExistingChatMessages :: Chatroom -> Maybe Int -> C.ConduitT () ChatMessage IO ()
getExistingChatMessages (Chatroom chatroomId _ _ _ getChatMessages _ _) =
  getChatMessages chatroomId

subscribeMessagesLive :: Chatroom -> IO (TChan ChatMessage)
subscribeMessagesLive (Chatroom chatroomId _ subChannel _ getChatMessages _ _) = do
  atomically (dupTChan subChannel)

processNextMessage :: Chatroom -> SendMessage -> IO ()
processNextMessage (Chatroom roomId sequenceWriteChannel broadcastChan persistChatMessage _ countChatMessages _) msg = do
  now <- getCurrentTime
  messageNumber <- (+ 1) <$> countChatMessages roomId
  let chatMessage = ChatMessage (senderUserId msg) (userDisplayName msg) (message msg) now messageNumber
  -- TODO: stop this loop from exiting due to IO errors
  persistChatMessage roomId chatMessage
  atomically (writeTChan broadcastChan chatMessage)

workerLoop :: Chatroom -> IO loop
workerLoop chatroom@(Chatroom _ sequenceWriteChannel _ _ _ _ _) = forever $ do
  msg <- atomically (readTChan sequenceWriteChannel)
  result <- try (processNextMessage chatroom msg)
  printAndIgnoreSyncException result

thawChatroom :: Chatroom -> IO ()
thawChatroom chatroom = startIfNotStarted (workerThread chatroom) (workerLoop chatroom)

freezeChatroom :: Chatroom -> IO ()
freezeChatroom chatroom = stopIfNotStopped (workerThread chatroom)
