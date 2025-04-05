module Controllers.Common.CacheableSharedResource (makeResourceCache, makeGlobalResourceCache, withCacheableResource, getCacheableResource, ResourceCache) where

import qualified Control.Foldl as Foldl
import qualified DeferredFolds.UnfoldlM as UnfoldlM

import ClassyPrelude (liftIO)
import Control.Concurrent
import Control.Concurrent.STM (TVar, modifyTVar)
import Control.Concurrent.STM.TVar
import Control.Exception (bracket_)
import Control.Monad
import Control.Monad.STM
import Data.Either
import Data.Int
import Data.Text
import Data.Time
import UnliftIO.Resource
import Prelude
import qualified StmContainers.Map as M

data CacheItem a = CacheItem { cacheItem :: a, connections :: TVar Int }

data ResourceCache err a = ResourceCache
  { cache :: (M.Map Text (CacheItem a)),
    cleanUpMap :: (M.Map Text UTCTime),
    loadResourceOp :: Text -> IO (Either err a),
    cacheCleanupThreadId :: ThreadId
  }

increaseSharersByOne :: CacheItem a -> STM Int
increaseSharersByOne (CacheItem item connections) = modifyTVar' connections (+1) >> readTVar connections

decreaseSharersByOne :: CacheItem a -> STM Int
decreaseSharersByOne (CacheItem item connections) = modifyTVar' connections (\item -> item - 1) >> readTVar connections

numberOfSharers :: CacheItem a -> STM Int
numberOfSharers (CacheItem item connections) = readTVar connections

makeResourceCache :: MonadResource m => (Text -> IO (Either err a)) -> m (ReleaseKey, ResourceCache err a)
makeResourceCache loadResourceOp = do
  allocate (allocateResource loadResourceOp) deallocateResource
  where
    allocateResource :: (Text -> IO (Either err a)) -> IO (ResourceCache err a)
    allocateResource loadResource = makeGlobalResourceCache loadResource

    deallocateResource :: ResourceCache err a -> IO ()
    deallocateResource cache = do
      let threadId = cacheCleanupThreadId cache
      killThread threadId

makeGlobalResourceCache :: (Text -> IO (Either err a)) -> IO (ResourceCache err a)
makeGlobalResourceCache loadResourceOp = do
  resourceCache <- M.newIO
  cleanUpMap <- M.newIO
  threadId <- forkIO $ startBroomLoop resourceCache cleanUpMap
  pure $ ResourceCache resourceCache cleanUpMap loadResourceOp threadId

startBroomLoop :: M.Map Text (CacheItem a) -> M.Map Text UTCTime -> IO ()
startBroomLoop resourceCache cleanUpMap = forever $ do
  now <- getCurrentTime
  potentiallyStaleMapItems <- atomically $ stmMapToList cleanUpMap

  forM_ potentiallyStaleMapItems $ \(resourceId, cacheExpiryTime) -> do
    when (now >= cacheExpiryTime) $ atomically $ removeIfNoSharers resourceCache cleanUpMap resourceId

  threadDelay (1 * 60000000)
  startBroomLoop resourceCache cleanUpMap

scheduleCacheCleanup :: ResourceCache err a -> UTCTime -> Text -> STM ()
scheduleCacheCleanup (ResourceCache cache cleanUpMap _ _) now resourceId = do
  let oneMinute = 60
  M.insert (addUTCTime oneMinute now) resourceId cleanUpMap

removeScheduledCleanup :: M.Map Text UTCTime -> Text -> STM ()
removeScheduledCleanup cleanUpMap resourceId =
  M.delete resourceId cleanUpMap

handlerSharerJoin :: ResourceCache err a -> CacheItem a -> Text -> STM ()
handlerSharerJoin cache cacheItem resourceId = do
  void $ increaseSharersByOne cacheItem
  removeScheduledCleanup (cleanUpMap cache) resourceId

handleSharerLeave :: ResourceCache err a -> CacheItem a -> Text -> IO ()
handleSharerLeave cache cacheItem resourceId = do
  now <- getCurrentTime
  atomically $ do
    newSharerCount <- decreaseSharersByOne cacheItem
    when (newSharerCount == 0) $ void (scheduleCacheCleanup cache now resourceId)

removeFromCache :: M.Map Text (CacheItem a) -> Text -> STM ()
removeFromCache resourceCache resourceId = M.delete resourceId resourceCache

removeIfNoSharers :: M.Map Text (CacheItem a) -> M.Map Text UTCTime -> Text -> STM ()
removeIfNoSharers cache cleanupMap resourceId = do
  resource <- M.lookup resourceId cache
  case resource of
    Nothing -> pure ()
    Just cachedItem -> do
      sharers <- numberOfSharers cachedItem
      when (sharers == 0) $ removeFromCache cache resourceId >> removeScheduledCleanup cleanupMap resourceId

loadCacheableResource :: ResourceCache err a -> Text -> IO (Either err (CacheItem a))
loadCacheableResource resourceCache@(ResourceCache cache _ loadResourceOp _) resourceId = do
  existingItem <- atomically $ do
    item <- M.lookup resourceId cache
    case item of
      Nothing -> pure Nothing
      Just resource -> do
        handlerSharerJoin resourceCache resource resourceId
        pure $ Just resource

  case existingItem of 
    Just item -> pure $ Right item
    Nothing -> do
      resource <- loadResourceOp resourceId
      case resource of
        Left loadError -> pure $ Left loadError
        Right newResource ->
          atomically $ do
            cachedItem <- M.lookup resourceId cache
            result <- case cachedItem of
              Just cachedResource -> pure cachedResource
              Nothing -> do
                numConnections <- newTVar 0
                let item = CacheItem newResource numConnections
                M.insert item resourceId cache
                pure item
            
            handlerSharerJoin resourceCache result resourceId
            pure (Right result)

stmMapToList :: M.Map k v -> STM [(k, v)]
stmMapToList = UnfoldlM.foldM (Foldl.generalize Foldl.list) . M.unfoldlM

withCacheableResource :: ResourceCache err a -> Text -> (Either err a -> IO ()) -> IO ()
withCacheableResource cache resourceId op = 
  runResourceT $ do
    (_, cachedResource) <- getCacheableResource cache resourceId
    liftIO $ op cachedResource

getCacheableResource :: MonadResource m =>ResourceCache err a -> Text -> m (ReleaseKey, Either err a)
getCacheableResource resourceCache resourceId = do
  (releaseKey, cacheEntry) <- allocate (allocateResource resourceCache resourceId) (deAllocateResource resourceCache)
  return (releaseKey, cacheItem <$> cacheEntry)

  where
    allocateResource :: ResourceCache err a -> Text -> IO (Either err (CacheItem a))
    allocateResource resourceCache resourceId = loadCacheableResource resourceCache resourceId

    deAllocateResource :: ResourceCache err a -> Either err (CacheItem a) -> IO ()
    deAllocateResource resourceCache resource = do
      case resource of
        Left _ -> pure ()
        Right sharedResource -> handleSharerLeave resourceCache sharedResource resourceId