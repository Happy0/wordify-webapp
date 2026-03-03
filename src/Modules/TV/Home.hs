{-# LANGUAGE ExistentialQuantification #-}

module Modules.TV.Home
  ( HomeTvService,
    HomeTvUpdate(..),
    makeHomeTvService,
    subscribeHomeTV,
    currentTVState
  )
where

import ClassyPrelude (IO, Int, pure, undefined, race_, (*), ($), writeTChan, (.))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan, newBroadcastTChan, dupTChan)
import Controllers.Game.Model.ServerGame (ServerGame (..), ServerGameSnapshot (..), makeServerGameSnapshot)
import Repository.GameRepository (GameRepository (getRecentlyActiveGames), GameSummary (..))
import GHC.Conc.IO (threadDelay)
import Control.Monad (forever)
import Modules.Games.Api (GameService, getGame)
import Control.Monad.Trans.Resource
import Data.Either (Either(..))
import ClassyPrelude.Yesod (MonadIO(liftIO))
import Wordify.Rules.Game (GameStatus(Finished), Game (gameStatus))
import Control.Monad (when)
import Controllers.Game.Api (GameMessage)
import Control.Concurrent.STM (readTChan)
import Controllers.GameLobby.Model.GameLobby (duplicateBroadcastChannel)
import ClassyPrelude

data HomeTvUpdate = HomeTVUpdate ServerGameSnapshot

data HomeTvService = forall a. GameRepository a => HomeTvService
  { homeTvServiceGameRepository :: a,
    homeTvServiceCurrentGame :: TVar (Maybe ServerGameSnapshot),
   homeTvServiceChannel :: TChan HomeTvUpdate
  }

makeHomeTvService :: GameRepository a => a -> GameService -> Int -> IO HomeTvService
makeHomeTvService repo gameService refreshAfterInactivityMinutes = do
  chan <- atomically newBroadcastTChan
  currentGameVar <- newTVarIO Nothing
  _ <- forkIO (runTvUpdateWorker repo gameService chan currentGameVar refreshAfterInactivityMinutes)
  pure (HomeTvService repo currentGameVar chan)

subscribeHomeTV :: HomeTvService -> IO (TChan HomeTvUpdate)
subscribeHomeTV = atomically . dupTChan . homeTvServiceChannel

currentTVState :: HomeTvService -> IO (Maybe ServerGameSnapshot)
currentTVState = readTVarIO . homeTvServiceCurrentGame

runTvUpdateWorker :: GameRepository a => a -> GameService -> TChan HomeTvUpdate -> TVar (Maybe ServerGameSnapshot) -> Int -> IO ()
runTvUpdateWorker repository gameService chan currentGameVar refreshAfterInactivityMinutes = 
  forever (race_ newGameSearchTimer (sendCurrentTvGameUpdates repository gameService chan currentGameVar))
  where
    newGameSearchTimer = threadDelay (1000000 * 60 * refreshAfterInactivityMinutes)

sendCurrentTvGameUpdates :: GameRepository a => a -> GameService -> TChan HomeTvUpdate -> TVar (Maybe ServerGameSnapshot) -> IO ()
sendCurrentTvGameUpdates repository gameService homeBroadcastChannel currentGameVar = runResourceT $ do
  latestServerGame <- getGameWithLatestActivity repository gameService
  case latestServerGame of
    -- No active games, return after a minute to see if there's been any new games started
    Nothing -> liftIO (threadDelay (1000000 * 60))
    Just serverGame-> do
      gameChannel <- liftIO (atomically (dupTChan (broadcastChannel serverGame)))
      liftIO (keepHomeChannelUpdated serverGame gameChannel homeBroadcastChannel currentGameVar)

keepHomeChannelUpdated :: ServerGame -> TChan GameMessage -> TChan HomeTvUpdate -> TVar (Maybe ServerGameSnapshot) -> IO ()
keepHomeChannelUpdated serverGame gameChannel broadcastChannel currentGameVar = do
  nextSnapshot <- atomically (makeServerGameSnapshot serverGame)
  _ <- atomically (makeServerGameSnapshot serverGame >>= writeTVar currentGameVar . Just)
  let game = gameState nextSnapshot

  case gameStatus game of 
    Finished -> do
      sendUpdate broadcastChannel nextSnapshot
      -- Return from function to allow next active game to come on the TV
      threadDelay (1000000 * 60)
      pure ()
    _ -> do
      sendUpdate broadcastChannel nextSnapshot
      -- Wait for next update
      _ <- atomically (readTChan gameChannel)
      keepHomeChannelUpdated serverGame gameChannel broadcastChannel currentGameVar
  where
    sendUpdate ::  TChan HomeTvUpdate -> ServerGameSnapshot-> IO ()
    sendUpdate chan = atomically . writeTChan chan . HomeTVUpdate 

getGameWithLatestActivity :: (MonadResource m, GameRepository a) => a -> GameService -> m (Maybe ServerGame)
getGameWithLatestActivity repository gameService = do
  games <- liftIO (getRecentlyActiveGames repository 1)
  case games of 
    [] -> pure Nothing
    (GameSummary gameId _ _ _ _ _): _ -> do
      (_, game) <- getGame gameService gameId
      case game of
        Left _ -> pure Nothing
        Right game -> pure (Just game)