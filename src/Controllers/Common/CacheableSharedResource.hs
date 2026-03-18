{-# LANGUAGE ScopedTypeVariables #-}
module Controllers.Common.CacheableSharedResource (
  makeResourceCache,
  makeGlobalResourceCache,
  withCacheableResource,
  getCacheableResource,
  ResourceCache,
  peekCacheableResource,
  withPeekCacheableResource) where

import ClassyPrelude (liftIO)
import Control.Concurrent
import Control.Concurrent.STM.TVar
import qualified Control.Foldl as Foldl
import Control.Monad
import Control.Monad.STM
import Data.Either
import Data.Int
import Data.Text
import Data.Time
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import qualified StmContainers.Map as M
import UnliftIO.Resource
import Prelude
import Control.Exception (bracket, catch, throwIO, SomeException)
import Control.Exception (finally)

data CacheItem a = CacheItem {cacheItem :: a, connections :: TVar Int}

data CacheEntry err a = LoadedEntry (CacheItem a) | LoadingEntry (MVar ())

data ResourceCache err a = ResourceCache
  { cache :: M.Map Text (CacheEntry err a),
    cleanUpMap :: M.Map Text UTCTime,
    loadResourceOp :: Text -> IO (Either err a),
    onRemoval :: Maybe (a -> IO ()),
    cacheCleanupThreadId :: ThreadId
  }

increaseSharersByOne :: CacheItem a -> STM Int
increaseSharersByOne (CacheItem item connections) = modifyTVar' connections (+ 1) >> readTVar connections

decreaseSharersByOne :: CacheItem a -> STM Int
decreaseSharersByOne (CacheItem item connections) = modifyTVar' connections (\item -> item - 1) >> readTVar connections

numberOfSharers :: CacheItem a -> STM Int
numberOfSharers (CacheItem item connections) = readTVar connections

makeResourceCache :: (MonadResource m) => (Text -> IO (Either err a)) -> Maybe (a -> IO ()) -> m (ReleaseKey, ResourceCache err a)
makeResourceCache loadResourceOp onRemoval = do
  allocate (allocateResource loadResourceOp onRemoval) deallocateResource
  where
    allocateResource :: (Text -> IO (Either err a)) -> Maybe (a -> IO ()) -> IO (ResourceCache err a)

    deallocateResource :: ResourceCache err a -> IO ()
    deallocateResource cache = do
      let threadId = cacheCleanupThreadId cache
      killThread threadId
    allocateResource = makeGlobalResourceCache

makeGlobalResourceCache :: (Text -> IO (Either err a)) -> Maybe (a -> IO ()) -> IO (ResourceCache err a)
makeGlobalResourceCache loadResourceOp onRemoval = do
  resourceCache <- M.newIO
  cleanUpMap <- M.newIO
  threadId <- forkIO $ startBroomLoop resourceCache cleanUpMap onRemoval
  pure $ ResourceCache resourceCache cleanUpMap loadResourceOp onRemoval threadId

startBroomLoop :: M.Map Text (CacheEntry err a) -> M.Map Text UTCTime -> Maybe (a -> IO ()) -> IO ()
startBroomLoop resourceCache cleanUpMap onRemove = forever $ do
  now <- getCurrentTime
  potentiallyStaleMapItems <- atomically $ stmMapToList cleanUpMap

  forM_ potentiallyStaleMapItems $ \(resourceId, cacheExpiryTime) -> do
    when (now >= cacheExpiryTime) $ do
      removed <- atomically $ removeIfNoSharers resourceCache cleanUpMap resourceId
      case (removed, onRemove) of
        (Just removedItem, Just onRemovalFunction) -> onRemovalFunction removedItem
        _ -> return ()

  threadDelay (1 * 60000000)

scheduleCacheCleanup :: ResourceCache err a -> UTCTime -> Text -> STM ()
scheduleCacheCleanup (ResourceCache cache cleanUpMap _ _ _) now resourceId = do
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
  atomically (handleSharerLeaveSTM cache cacheItem resourceId now)

handleSharerLeaveSTM  :: ResourceCache err a -> CacheItem a -> Text -> UTCTime -> STM ()
handleSharerLeaveSTM cache cacheItem resourceId now = do
  newSharerCount <- decreaseSharersByOne cacheItem
  when (newSharerCount == 0) $ void (scheduleCacheCleanup cache now resourceId)

removeFromCache :: M.Map Text (CacheEntry err a) -> Text -> STM ()
removeFromCache resourceCache resourceId = M.delete resourceId resourceCache

removeIfNoSharers :: M.Map Text (CacheEntry err a) -> M.Map Text UTCTime -> Text -> STM (Maybe a)
removeIfNoSharers cache cleanupMap resourceId = do
  resource <- M.lookup resourceId cache
  case resource of
    Just (LoadedEntry cached) -> do
      sharers <- numberOfSharers cached

      if sharers == 0
        then do
          removeFromCache cache resourceId
          removeScheduledCleanup cleanupMap resourceId
          return (Just (cacheItem cached))
        else
          return Nothing
    _ -> pure Nothing

loadCacheableResource :: ResourceCache err a -> Text -> IO (Either err (CacheItem a))
loadCacheableResource resourceCache@(ResourceCache cache _ loadResourceOp _ _) resourceId = do
  existingItem <- atomically $ do
    item <- M.lookup resourceId cache
    case item of
      Nothing -> pure Nothing
      Just result@(LoadingEntry loadingMVar) -> pure (Just result)
      Just result@(LoadedEntry resource) -> do
        handlerSharerJoin resourceCache resource resourceId
        pure (Just result)

  case existingItem of
    Just (LoadedEntry item) -> pure $ Right item
    Just (LoadingEntry loadedSignalMVar) -> do
      -- Wait the for the other thread that is already loading the resource to signal that it has loaded the item
      -- into the cache then recursively start again
      readMVar loadedSignalMVar
      loadCacheableResource resourceCache resourceId
    Nothing -> loadFreshlyIntoCache resourceCache resourceId

loadFreshlyIntoCache :: ResourceCache err a -> Text -> IO (Either err (CacheItem a))
loadFreshlyIntoCache resourceCache@(ResourceCache cache _ loadResourceOp _ _) resourceId = do
  maybeSemaphore <- takeOwnershipOfLoad resourceCache resourceId

  case maybeSemaphore of 
    -- Already loaded or loading in another thread, recursively start again
    Nothing -> loadCacheableResource resourceCache resourceId

    -- We've claimed ownership of loading the item into the cache
    Just semaphore -> loadIntoCache resourceCache resourceId semaphore

  where
    -- Returns a Just if we took ownership and Nothing if a thread is already loading the item or it has already been fully loaded
    takeOwnershipOfLoad :: ResourceCache err a -> Text -> IO (Maybe (MVar ()))
    takeOwnershipOfLoad resourceCache@(ResourceCache cache _ loadResourceOp _ _) resourceId = do
      signal <- newEmptyMVar
      atomically $ do
        cachedItem <- M.lookup resourceId cache
        case cachedItem of 
          Nothing -> do 
            let entry = LoadingEntry signal 
            M.insert entry resourceId cache
            pure (Just signal)
          _ -> pure Nothing

    loadIntoCache :: ResourceCache err a -> Text -> MVar () -> IO (Either err (CacheItem a))
    loadIntoCache resourceCache@(ResourceCache cache _ loadResourceOp _ _) resourceId signalMVar = do
      resourceLoadResult <- loadResourceOp resourceId 
        `catch` (\(err :: SomeException) -> removeLoadingClaim resourceCache resourceId >> putMVar signalMVar () >> throwIO err)

      result <- case resourceLoadResult of 
        Right resource -> putIntoCache resourceCache resourceId resource
        Left err -> removeLoadingClaim resourceCache resourceId >> pure (Left err)

      putMVar signalMVar ()
      pure result

    putIntoCache :: ResourceCache err a -> Text -> a -> IO (Either err (CacheItem a))
    putIntoCache resourceCache@(ResourceCache cache _ loadResourceOp _ _) resourceId resource = do
      atomically $ do
        connections <- newTVar 0
        let entry = CacheItem resource connections
        handlerSharerJoin resourceCache entry resourceId
        M.insert (LoadedEntry entry) resourceId cache
        pure (Right entry)

    removeLoadingClaim :: ResourceCache err a -> Text -> IO ()
    removeLoadingClaim resourceCache@(ResourceCache cache _ _ _ _) resourceId = atomically $ M.delete resourceId cache

stmMapToList :: M.Map k v -> STM [(k, v)]
stmMapToList = UnfoldlM.foldM (Foldl.generalize Foldl.list) . M.unfoldlM

withCacheableResource :: ResourceCache err a -> Text -> (Either err a -> IO ()) -> IO ()
withCacheableResource cache resourceId op =
  runResourceT $ do
    (_, cachedResource) <- getCacheableResource cache resourceId
    liftIO $ op cachedResource

getCacheableResource :: (MonadResource m) => ResourceCache err a -> Text -> m (ReleaseKey, Either err a)
getCacheableResource resourceCache resourceId = do
  (releaseKey, cacheEntry) <- allocate (allocateResource resourceCache resourceId) (deAllocateResource resourceCache)
  return (releaseKey, cacheItem <$> cacheEntry)
  where
    allocateResource :: ResourceCache err a -> Text -> IO (Either err (CacheItem a))
    allocateResource = loadCacheableResource

    deAllocateResource :: ResourceCache err a -> Either err (CacheItem a) -> IO ()
    deAllocateResource resourceCache resource = do
      case resource of
        Left _ -> pure ()
        Right sharedResource -> handleSharerLeave resourceCache sharedResource resourceId

{-
  Returns the item if it's in the cache but does not load it into the cache if it is not present.

  If there are no other sharers of the resource once the resource action has been run to completion
  or the 'release key' is used to free it early then the item is scheduled for eviction from the cache.
-}
peekCacheableResource :: MonadResource m => ResourceCache err a -> Text -> m (ReleaseKey, Maybe a)
peekCacheableResource resourceCache resourceId = do
  (releaseKey, cacheEntry) <- allocate (allocateResource resourceCache resourceId) (deAllocateResource resourceCache)
  return (releaseKey, cacheItem <$> cacheEntry)
  where
    allocateResource :: ResourceCache err a -> Text -> IO (Maybe (CacheItem a))
    allocateResource resourceCache resourceId = atomically $ do
      item <- M.lookup resourceId (cache resourceCache)
      case item of
        Just (LoadedEntry cachedItem) -> do
          handlerSharerJoin resourceCache cachedItem resourceId
          pure (Just cachedItem)
        _ -> pure Nothing    

    deAllocateResource :: ResourceCache err a -> Maybe (CacheItem a) -> IO ()
    deAllocateResource resourceCache resource =
      case resource of
        Nothing -> pure ()
        Just sharedResource -> handleSharerLeave resourceCache sharedResource resourceId

{-
  Executes the given action with a 'Just' if an item with the given resourceID is present in the cache,
  otherwise executes it with Nothing

  If there are no other sharers of the resource once the action is complete then the item is scheduled
  for eviction

  The date parameter is used to schedule the item for eviction from the cache.
-}
withPeekCacheableResource :: ResourceCache err a -> Text -> (Maybe a -> STM b) -> UTCTime -> STM b
withPeekCacheableResource resourceCache resourceId action now = do
  resource <- M.lookup resourceId (cache resourceCache)
  case resource of
    Just (LoadedEntry item) -> do
      handlerSharerJoin resourceCache item resourceId
      result <- action (Just (cacheItem item))
      handleSharerLeaveSTM resourceCache item resourceId now
      pure result
    _ -> action Nothing

