module Controllers.Common.CacheableSharedResource (makeResourceCache, makeGlobalResourceCache, withCacheableResource, getCacheableResource, ResourceCache, CacheableSharedResource (decreaseSharersByOne, increaseSharersByOne, numberOfSharers)) where

import Control.Concurrent
import Control.Concurrent.STM (TVar, modifyTVar)
import Control.Concurrent.STM.TVar
import Control.Exception (bracket_)
import Control.Monad
import Control.Monad.Cont (liftIO)
import Control.Monad.STM
import Data.Either
import Data.Int
import Data.Map as M
import Data.Text
import Data.Time
import UnliftIO.Resource
import Prelude

data ResourceCache err a = ResourceCache
  { cache :: TVar (Map Text a),
    cleanUpMap :: TVar (Map Text UTCTime),
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
  resourceCache <- newTVarIO M.empty
  cleanUpMap <- newTVarIO M.empty
  threadId <- forkIO $ startBroomLoop resourceCache cleanUpMap
  pure $ ResourceCache resourceCache cleanUpMap loadResourceOp threadId

startBroomLoop :: CacheableSharedResource a => TVar (Map Text a) -> TVar (Map Text UTCTime) -> IO ()
startBroomLoop resourceCache cleanUpMap = forever $ do
  now <- getCurrentTime
  potentiallyStaleItems <- readTVarIO cleanUpMap

  let potentiallyStaleItemsList = M.toList potentiallyStaleItems
  forM_ potentiallyStaleItemsList $ \(resourceId, cacheExpiryTime) -> do
    when (now >= cacheExpiryTime) $ atomically $ removeIfNoSharers resourceCache cleanUpMap resourceId

  threadDelay (1 * 60000000)
  startBroomLoop resourceCache cleanUpMap

scheduleCacheCleanup :: ResourceCache err a -> UTCTime -> Text -> STM ()
scheduleCacheCleanup (ResourceCache cache cleanUpMap _ _) now resourceId = do
  let oneMinute = 60
  modifyTVar' cleanUpMap (M.insert resourceId (addUTCTime oneMinute now))

removeScheduledCleanup :: TVar (Map Text UTCTime) -> Text -> STM ()
removeScheduledCleanup cleanUpMap resourceId =
  modifyTVar' cleanUpMap (M.delete resourceId)

handlerSharerJoin :: CacheableSharedResource a => ResourceCache err a -> a -> Text -> IO ()
handlerSharerJoin cache resource resourceId =
  atomically $ do
    void $ increaseSharersByOne resource
    removeScheduledCleanup (cleanUpMap cache) resourceId

handleSharerLeave :: CacheableSharedResource a => ResourceCache err a -> a -> Text -> IO ()
handleSharerLeave cache resource resourceId = do
  now <- getCurrentTime
  atomically $ do
    newSharerCount <- decreaseSharersByOne resource
    when (newSharerCount == 0) $ void (scheduleCacheCleanup cache now resourceId)

removeFromCache :: CacheableSharedResource a => TVar (Map Text a) -> Text -> STM ()
removeFromCache resourceCache resourceId = void $ M.delete resourceId <$> readTVar resourceCache

removeIfNoSharers :: CacheableSharedResource a => TVar (Map Text a) -> TVar (Map Text UTCTime) -> Text -> STM ()
removeIfNoSharers cache cleanupMap resourceId = do
  cachedItems <- readTVar cache
  let resource = M.lookup resourceId cachedItems
  case resource of
    Nothing -> pure ()
    Just cachedItem -> do
      sharers <- numberOfSharers cachedItem
      when (sharers == 0) $ removeFromCache cache resourceId >> removeScheduledCleanup cleanupMap resourceId

useCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> Either err a -> (Either err a -> IO c) -> IO c
useCacheableResource (ResourceCache cache _ _ _) resourceId (Left resourceLoadErr) operation =
  operation
    (Left resourceLoadErr)
useCacheableResource resourceCache@(ResourceCache cache _ _ _) resourceId (Right resource) operation =
  bracket_ (handlerSharerJoin resourceCache resource resourceId) (handleSharerLeave resourceCache resource resourceId) (operation (Right resource))

loadCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> IO (Either err a)
loadCacheableResource (ResourceCache cache _ loadResourceOp _) resourceId = do
  resource <- loadResourceOp resourceId
  case resource of
    Left loadError -> pure $ Left loadError
    Right cachedResource ->
      atomically $ do
        cachedItem <- M.lookup resourceId <$> readTVar cache
        case cachedItem of
          Just cachedResource -> pure $ Right cachedResource
          Nothing -> do
            modifyTVar cache $ M.insert resourceId cachedResource
            pure $ Right cachedResource

withCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> (Either err a -> IO ()) -> IO ()
withCacheableResource cache resourceId op = do
  resource <- loadCacheableResource cache resourceId
  useCacheableResource cache resourceId resource op

getCacheableResource :: MonadResource m => CacheableSharedResource a => ResourceCache err a -> Text -> m (ReleaseKey, Either err a)
getCacheableResource resourceCache resourceId =
  allocate (allocateResource resourceCache resourceId) (deAllocateResource resourceCache)
  where
    allocateResource :: CacheableSharedResource a => ResourceCache err a -> Text -> IO (Either err a)
    allocateResource resourceCache resourceId = do
      loadResult <- loadCacheableResource resourceCache resourceId

      case loadResult of
        Right resource -> handlerSharerJoin resourceCache resource resourceId >> pure loadResult
        Left err -> pure $ Left err

    deAllocateResource :: CacheableSharedResource a => ResourceCache err a -> Either err a -> IO ()
    deAllocateResource resourceCache resource = do
      case resource of
        Left _ -> pure ()
        Right sharedResource -> handleSharerLeave resourceCache sharedResource resourceId