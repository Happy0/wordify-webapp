module Controllers.Common.CacheableSharedResource (makeResourceCache, makeGlobalResourceCache, withCacheableResource, getCacheableResource, ResourceCache, CacheableSharedResource (decreaseSharersByOne, increaseSharersByOne, numberOfSharers)) where

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

data ResourceCache err a = ResourceCache
  { cache :: (M.Map Text a),
    cleanUpMap :: (M.Map Text UTCTime),
    loadResourceOp :: Text -> IO (Either err a),
    cacheCleanupThreadId :: ThreadId
  }

class CacheableSharedResource a where
  decreaseSharersByOne :: a -> STM Int
  increaseSharersByOne :: a -> STM Int
  numberOfSharers :: a -> STM Int

makeResourceCache :: MonadResource m => CacheableSharedResource a => (Text -> IO (Either err a)) -> m (ReleaseKey, ResourceCache err a)
makeResourceCache loadResourceOp = do
  allocate (allocateResource loadResourceOp) deallocateResource
  where
    allocateResource :: CacheableSharedResource a => (Text -> IO (Either err a)) -> IO (ResourceCache err a)
    allocateResource loadResource = makeGlobalResourceCache loadResource

    deallocateResource :: CacheableSharedResource a => ResourceCache err a -> IO ()
    deallocateResource cache = do
      let threadId = cacheCleanupThreadId cache
      killThread threadId

makeGlobalResourceCache :: CacheableSharedResource a => (Text -> IO (Either err a)) -> IO (ResourceCache err a)
makeGlobalResourceCache loadResourceOp = do
  resourceCache <- M.newIO
  cleanUpMap <- M.newIO
  threadId <- forkIO $ startBroomLoop resourceCache cleanUpMap
  pure $ ResourceCache resourceCache cleanUpMap loadResourceOp threadId

startBroomLoop :: CacheableSharedResource a => M.Map Text a -> M.Map Text UTCTime -> IO ()
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

handlerSharerJoin :: CacheableSharedResource a => ResourceCache err a -> a -> Text -> STM ()
handlerSharerJoin cache resource resourceId = do
  void $ increaseSharersByOne resource
  removeScheduledCleanup (cleanUpMap cache) resourceId

handleSharerLeave :: CacheableSharedResource a => ResourceCache err a -> a -> Text -> IO ()
handleSharerLeave cache resource resourceId = do
  now <- getCurrentTime
  atomically $ do
    newSharerCount <- decreaseSharersByOne resource
    when (newSharerCount == 0) $ void (scheduleCacheCleanup cache now resourceId)

removeFromCache :: CacheableSharedResource a => M.Map Text a -> Text -> STM ()
removeFromCache resourceCache resourceId = M.delete resourceId resourceCache

removeIfNoSharers :: CacheableSharedResource a => M.Map Text a -> M.Map Text UTCTime -> Text -> STM ()
removeIfNoSharers cache cleanupMap resourceId = do
  resource <- M.lookup resourceId cache
  case resource of
    Nothing -> pure ()
    Just cachedItem -> do
      sharers <- numberOfSharers cachedItem
      when (sharers == 0) $ removeFromCache cache resourceId >> removeScheduledCleanup cleanupMap resourceId

loadCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> IO (Either err a)
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
        Right cachedResource ->
          atomically $ do
            cachedItem <- M.lookup resourceId cache
            result <- case cachedItem of
              Just cachedResource -> pure cachedResource
              Nothing -> do
                M.insert cachedResource resourceId cache
                pure cachedResource
            
            handlerSharerJoin resourceCache result resourceId
            pure (Right result)

withCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> (Either err a -> IO ()) -> IO ()
withCacheableResource cache resourceId op = 
  runResourceT $ do
    (_, cachedResource) <- getCacheableResource cache resourceId
    liftIO $ op cachedResource

getCacheableResource :: MonadResource m => CacheableSharedResource a => ResourceCache err a -> Text -> m (ReleaseKey, Either err a)
getCacheableResource resourceCache resourceId =
  allocate (allocateResource resourceCache resourceId) (deAllocateResource resourceCache)
  where
    allocateResource :: CacheableSharedResource a => ResourceCache err a -> Text -> IO (Either err a)
    allocateResource resourceCache resourceId = loadCacheableResource resourceCache resourceId

    deAllocateResource :: CacheableSharedResource a => ResourceCache err a -> Either err a -> IO ()
    deAllocateResource resourceCache resource = do
      case resource of
        Left _ -> pure ()
        Right sharedResource -> handleSharerLeave resourceCache sharedResource resourceId

stmMapToList :: M.Map k v -> STM [(k, v)]
stmMapToList = UnfoldlM.foldM (Foldl.generalize Foldl.list) . M.unfoldlM