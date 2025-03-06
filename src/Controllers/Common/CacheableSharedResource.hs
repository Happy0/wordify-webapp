module Controllers.Common.CacheableSharedResource (makeResourceCache, withCacheableResource, getCacheableResource, ResourceCache (ResourceCache), CacheableSharedResource (decreaseSharersByOne, increaseSharersByOne, numberOfSharers)) where

import Control.Concurrent
import Control.Concurrent.STM (TVar, modifyTVar)
import Control.Concurrent.STM.TVar
import Control.Exception (bracket_)
import Control.Monad
import Control.Monad.STM
import Data.Either
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Pool
import Data.Text
import Data.Time
import Database.Persist.Sql (SqlBackend)
import Prelude

data ResourceCache err a = ResourceCache {cache :: TVar (Map Text a), cleanUpMap :: TVar (Map Text UTCTime), loadResourceOp :: Text -> IO (Either err a)}

class CacheableSharedResource a where
  decreaseSharersByOne :: a -> STM Int
  increaseSharersByOne :: a -> STM Int
  numberOfSharers :: a -> STM Int

makeResourceCache :: CacheableSharedResource a => (Text -> IO (Either err a)) -> IO (ResourceCache err a)
makeResourceCache loadResourceOp = do
  resourceCache <- newTVarIO M.empty
  cleanUpMap <- newTVarIO M.empty
  let cache = ResourceCache resourceCache cleanUpMap loadResourceOp
  _ <- forkIO $ startBroomLoop cache
  pure cache

startBroomLoop :: CacheableSharedResource a => ResourceCache err a -> IO ()
startBroomLoop resourceCache = forever $ do
  now <- getCurrentTime
  potentiallyStaleItems <- readTVarIO (cleanUpMap resourceCache)

  let potentiallyStaleItemsList = M.toList potentiallyStaleItems
  forM_ potentiallyStaleItemsList $ \(resourceId, cacheExpiryTime) -> do
    when (now >= cacheExpiryTime) $ atomically $ removeIfNoSharers resourceCache resourceId

  threadDelay (1 * 60000000)
  startBroomLoop resourceCache

scheduleCacheCleanup :: ResourceCache err a -> UTCTime -> Text -> STM ()
scheduleCacheCleanup (ResourceCache cache cleanUpMap _) now resourceId = do
  let oneMinute = 60
  modifyTVar' cleanUpMap (M.insert resourceId (addUTCTime oneMinute now))

removeScheduledCleanup :: ResourceCache err a -> Text -> STM ()
removeScheduledCleanup (ResourceCache cache cleanUpMap _) resourceId =
  modifyTVar' cleanUpMap (M.delete resourceId)

handlerSharerJoin :: CacheableSharedResource a => ResourceCache err a -> a -> Text -> IO ()
handlerSharerJoin cache resource resourceId =
  atomically $ do
    void $ increaseSharersByOne resource
    removeScheduledCleanup cache resourceId

handleSharerLeave :: CacheableSharedResource a => ResourceCache err a -> a -> Text -> IO ()
handleSharerLeave cache resource resourceId = do
  now <- getCurrentTime
  atomically $ do
    newSharerCount <- decreaseSharersByOne resource
    when (newSharerCount == 0) $ void (scheduleCacheCleanup cache now resourceId)

removeFromCache :: CacheableSharedResource a => ResourceCache err a -> Text -> STM ()
removeFromCache resourceCache resourceId = void $ M.delete resourceId <$> readTVar (cache resourceCache)

removeIfNoSharers :: CacheableSharedResource a => ResourceCache err a -> Text -> STM ()
removeIfNoSharers resourceCache@(ResourceCache cache cleanUpMap _) resourceId = do
  cachedItems <- readTVar cache
  let resource = M.lookup resourceId cachedItems
  case resource of
    Nothing -> pure ()
    Just cachedItem -> do
      sharers <- numberOfSharers cachedItem
      when (sharers == 0) $ removeFromCache resourceCache resourceId >> removeScheduledCleanup resourceCache resourceId

useCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> Either err a -> (Either err a -> IO c) -> IO c
useCacheableResource (ResourceCache cache _ _) resourceId (Left resourceLoadErr) operation =
  operation
    (Left resourceLoadErr)
useCacheableResource resourceCache@(ResourceCache cache _ _) resourceId (Right resource) operation =
  bracket_ (handlerSharerJoin resourceCache resource resourceId) (handleSharerLeave resourceCache resource resourceId) (operation (Right resource))

loadCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> IO (Either err a)
loadCacheableResource (ResourceCache cache _ loadResourceOp) resourceId = do
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

withCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> (Either err a -> IO c) -> IO c
withCacheableResource cache resourceId op = do
  resource <- loadCacheableResource cache resourceId
  useCacheableResource cache resourceId resource op

getCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> IO (Either err a)
getCacheableResource resourceCache resourceId = do
  resource <- loadCacheableResource resourceCache resourceId
  case resource of
    Left err -> pure $ Left err
    Right resource ->
      bracket_ (handlerSharerJoin resourceCache resource resourceId) (handleSharerLeave resourceCache resource resourceId) (pure (Right resource))
