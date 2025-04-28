module Util.WorkerThread (WorkerThread, newUnstartedWorkerThread, startIfNotStarted, stopIfNotStopped) where

import ClassyPrelude (Bool (False, True), IO, TVar, atomically, pure, readTVar, readTVarIO, when, writeTVar, ($))
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (STM, newTVarIO)
import Data.Foldable (for_)
import Data.Maybe (Maybe (Just, Nothing))

data WorkerThread = WorkerThread {started :: TVar Bool, threadId :: TVar (Maybe ThreadId)}

newUnstartedWorkerThread :: IO WorkerThread
newUnstartedWorkerThread = do
  started <- newTVarIO False
  threadId <- newTVarIO Nothing
  pure (WorkerThread started threadId)

startIfNotStarted :: WorkerThread -> IO () -> IO ()
startIfNotStarted thread@(WorkerThread _ threadId) workerAction = do
  threadNeedsStarted <- claimedAsStarted thread
  when threadNeedsStarted $ do
    workerThreadId <- forkIO workerAction
    atomically (writeTVar threadId (Just workerThreadId))

stopIfNotStopped :: WorkerThread -> IO ()
stopIfNotStopped workerThread = do
  workerThreadId <- atomically $ stopWorkerTransaction workerThread
  for_ workerThreadId killThread

claimedAsStarted :: WorkerThread -> IO Bool
claimedAsStarted (WorkerThread started _) =
  atomically $ do
    isAlreadyStarted <- readTVar started
    if isAlreadyStarted
      then pure False
      else do
        writeTVar started True
        pure True

stopWorkerTransaction :: WorkerThread -> STM (Maybe ThreadId)
stopWorkerTransaction (WorkerThread started threadId) = do
  writeTVar started False
  workerThreadId <- readTVar threadId
  writeTVar threadId Nothing
  pure workerThreadId