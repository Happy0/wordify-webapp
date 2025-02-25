module InactivityTracker where
    
    import Data.Time.Clock.POSIX
    import Control.Concurrent
    import Prelude
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.STM
    import System.Exit
    import Control.Monad
    import Control.Exception
    import Control.Monad.Loops
    import Yesod.WebSockets
    
    data InactivityTracker = InactivityTracker { lastRequestSecondsSinceUnixEpoch :: POSIXTime, openWebsockets :: Int }

    appIsInactive :: InactivityTracker -> POSIXTime -> Int -> Bool 
    appIsInactive (InactivityTracker lastRequestSecondsSinceUnixEpoch openWebsockets) currentSecondsSinceUnixEpoch shutdownAfterMinutesInactive = 
        case openWebsockets of 
            0 -> (floor (currentSecondsSinceUnixEpoch - lastRequestSecondsSinceUnixEpoch)) >= (shutdownAfterMinutesInactive * 60)
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
        bracket
            (atomically $ modifyTVar' inactivityTracker trackWebsocketConnect)
            (\_ -> do 
                atomically $ modifyTVar' inactivityTracker trackWebsocketDisconnect
                state <- readTVarIO inactivityTracker
                putStrLn ("number of websocket connections: " ++ ( show $ openWebsockets state))
            )
            (\_ -> action)
    
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

            case (appIsInactive inactivityState now shutdownAfterMinutesInactive) of
                True -> putStrLn "Exiting due to app inactivity" >> return ()
                False -> pollUntilAppInactive inActivityTrackerTvar shutdownAfterMinutesInactive

    raceUntilInactive :: (TVar InactivityTracker) -> IO () -> IO ()
    raceUntilInactive inactivityTracker action = 
        race_ (pollUntilAppInactive inactivityTracker 1) action

