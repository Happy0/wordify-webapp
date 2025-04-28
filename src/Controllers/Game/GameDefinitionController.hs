module Controllers.Game.GameDefinitionController (GameDefinitionController, DefinitionResponse (DefinitionResponse), makeGameDefinitionController, storeGameDefinitions, killWorkerThread, getStoredDefinitions) where

import ClassyPrelude (Either (Left, Right), IO, Int, UTCTime, getCurrentTime, map, pure, undefined, void, writeTQueue, ($), (+), (<$>))
import Conduit (ConduitT)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TQueue, atomically, newTQueue, readTQueue)
import Control.Monad (forever)
import Controllers.Definition.DefinitionService (Definition (Definition), DefinitionServiceImpl, getDefinitionsImpl)
import Data.Text (Text)
import Repository.DefinitionRepository (DefinitionRepository (getDefinitions), DefinitionRepositoryImpl, GameWordItem, WordDefinitionItem (WordDefinitionItem), countGameWordDefinitionsImpl, getGameDefinitionsImpl, saveGameDefinitionsImpl)
import Util.WorkerThread (WorkerThread, newUnstartedWorkerThread, startIfNotStarted, stopIfNotStopped)

data DefinitionResponse = DefinitionResponse Text UTCTime [Definition] Int

data GameDefinitionWorkItem = GameDefinitionWorkItem {gameId :: Text, word :: Text, definitions :: Either Text [Definition], withStoredResult :: DefinitionResponse -> IO ()}

data GameDefinitionController = GameDefinitionController
  { definitionService :: DefinitionServiceImpl,
    definitionRepository :: DefinitionRepositoryImpl,
    workQueue :: TQueue GameDefinitionWorkItem,
    defintionWorker :: WorkerThread
  }

makeGameDefinitionController :: DefinitionServiceImpl -> DefinitionRepositoryImpl -> IO GameDefinitionController
makeGameDefinitionController definitionService definitionRepository = do
  queue <- atomically newTQueue
  workerThread <- atomically newUnstartedWorkerThread
  let gameDefinitionWorker = GameDefinitionController definitionService definitionRepository queue workerThread
  pure gameDefinitionWorker

definitionWorkerLoop :: GameDefinitionController -> IO ()
definitionWorkerLoop (GameDefinitionController definitionService definitionRepository queue _) = forever $ do
  now <- getCurrentTime
  GameDefinitionWorkItem gameId word defs withStoredResult <- atomically $ readTQueue queue

  -- TODO: handle error on save killing thread
  case defs of
    Left err -> do
      -- todo: deal with code duplication by using 'either' function to default to empty list
      saveGameDefinitionsImpl definitionRepository now gameId word []
      definitionNum <- (+ 1) <$> countGameWordDefinitionsImpl definitionRepository gameId
      withStoredResult (DefinitionResponse word now [] definitionNum)
    Right definitions -> do
      saveGameDefinitionsImpl definitionRepository now gameId word (map wordDefinitionItem definitions)
      definitionNum <- (+ 1) <$> countGameWordDefinitionsImpl definitionRepository gameId
      withStoredResult (DefinitionResponse word now definitions definitionNum)
  where
    wordDefinitionItem :: Definition -> WordDefinitionItem
    wordDefinitionItem (Definition partOfSpeech definition example) = WordDefinitionItem partOfSpeech definition example

storeGameDefinitions :: GameDefinitionController -> Text -> Text -> (DefinitionResponse -> IO ()) -> IO ()
storeGameDefinitions worker@(GameDefinitionController definitionService _ workQueue definitionWorker) gameId word withStoredResultAsync = do
  startIfNotStarted definitionWorker (definitionWorkerLoop worker)

  void $ forkIO $ do
    defs <- getDefinitionsImpl definitionService word
    atomically (writeTQueue workQueue (GameDefinitionWorkItem gameId word defs withStoredResultAsync))

getStoredDefinitions :: GameDefinitionController -> Text -> ConduitT () GameWordItem IO ()
getStoredDefinitions (GameDefinitionController _ definitionRepository _ _) word = getGameDefinitionsImpl definitionRepository word

killWorkerThread :: GameDefinitionController -> IO ()
killWorkerThread (GameDefinitionController _ _ _ workerThread) = do
  stopIfNotStopped workerThread