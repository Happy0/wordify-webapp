module InactivityTracker where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Time.Clock.POSIX
import System.Exit
import Yesod.WebSockets
import Prelude

data InactivityTracker = InactivityTracker {lastRequestSecondsSinceUnixEpoch :: POSIXTime, openWebsockets :: Int}

appIsInactive :: InactivityTracker -> POSIXTime -> Int -> Bool
appIsInactive (InactivityTracker lastRequestSecondsSinceUnixEpoch openWebsockets) currentSecondsSinceUnixEpoch shutdownAfterMinutesInactive =
  case openWebsockets of
    0 -> floor (currentSecondsSinceUnixEpoch - lastRequestSecondsSinceUnixEpoch) >= (shutdownAfterMinutesInactive * 60)
    _ -> False

trackWebsocketConnect :: InactivityTracker -> InactivityTracker
trackWebsocketConnect (InactivityTracker lastRequestSecondsSinceUnixEpoch openWebsockets) =
  InactivityTracker lastRequestSecondsSinceUnixEpoch (openWebsockets + 1)

trackWebsocketDisconnect :: InactivityTracker -> InactivityTracker
trackWebsocketDisconnect (InactivityTracker lastRequestSecondsSinceUnixEpoch openWebsockets) =
  InactivityTracker lastRequestSecondsSinceUnixEpoch (openWebsockets - 1)

trackRequestReceived :: POSIXTime -> InactivityTracker -> InactivityTracker
trackRequestReceived currentSecondsSinceUnixEpoch activityState =
  InactivityTracker currentSecondsSinceUnixEpoch (openWebsockets activityState)

trackRequestReceivedActivity :: TVar InactivityTracker -> IO ()
trackRequestReceivedActivity inactivityTracker =
  do
    secondsSinceUnixEpoch <- getPOSIXTime
    atomically $ modifyTVar' inactivityTracker (trackRequestReceived secondsSinceUnixEpoch)

withTrackWebsocketActivity :: TVar InactivityTracker -> IO () -> IO ()
withTrackWebsocketActivity inactivityTracker action = do
  bracket_
    (atomically $ modifyTVar' inactivityTracker trackWebsocketConnect)
    (atomically $ modifyTVar' inactivityTracker trackWebsocketDisconnect)
    action

makeInactivityTracker :: IO (TVar InactivityTracker)
makeInactivityTracker =
  do
    secondsSinceUnixEpoch <- getPOSIXTime
    newTVarIO (InactivityTracker secondsSinceUnixEpoch 0)

pollUntilAppInactive :: TVar InactivityTracker -> Int -> IO ()
pollUntilAppInactive inActivityTrackerTvar shutdownAfterMinutesInactive =
  do
    _ <- threadDelay (shutdownAfterMinutesInactive * 60000000)
    now <- getPOSIXTime
    inactivityState <- readTVarIO inActivityTrackerTvar

    if appIsInactive inactivityState now shutdownAfterMinutesInactive
      then putStrLn "Exiting due to app inactivity"
      else pollUntilAppInactive inActivityTrackerTvar shutdownAfterMinutesInactive

raceUntilInactive :: TVar InactivityTracker -> IO () -> IO ()
raceUntilInactive inactivityTracker = race_ (pollUntilAppInactive inactivityTracker 1)
