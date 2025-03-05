module Controllers.Common.CacheableSharedResource (makeResourceCache, withCacheableResource, ResourceCache (ResourceCache), CacheableSharedResource (decreaseSharersByOne, increaseSharersByOne, numberOfSharers)) where

import Control.Concurrent.STM (TVar, modifyTVar)
import Control.Concurrent.STM.TVar
import Control.Exception (bracket_)
import Control.Monad
import Control.Monad.STM
import Data.Either
import Data.Int
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

-- Do - make timer task that periodically cleans up records on inactivity and add to cleanUpMap when 'shares' reaches 0
makeResourceCache :: (Text -> IO (Either err a)) -> IO (ResourceCache err a)
makeResourceCache loadResourceOp = do
  resourceCache <- newTVarIO M.empty
  cleanUpMap <- newTVarIO M.empty
  pure $ ResourceCache resourceCache cleanUpMap loadResourceOp

useCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> a -> (a -> IO ()) -> IO ()
useCacheableResource (ResourceCache cache _ _) resourceId resource operation =
  bracket_
    (handlerSharerJoin resource)
    (operation resource)
    (handleSharerLeave cache resource resourceId)
  where
    handlerSharerJoin = atomically . increaseSharersByOne
    handleSharerLeave cache resource resourceId = atomically $ do
      newSharerCount <- decreaseSharersByOne resource
      when (newSharerCount == 0) $ void (removeFromCache cache resourceId)
    removeFromCache cache resourceId = M.delete resourceId <$> readTVar cache

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

withCacheableResource :: CacheableSharedResource a => ResourceCache err a -> Text -> (a -> IO ()) -> IO (Either err ())
withCacheableResource cache resourceId op = do
  resource <- loadCacheableResource cache resourceId
  case resource of
    Left err -> pure (Left err)
    Right loadedResource -> do
      useCacheableResource cache resourceId loadedResource op
      return (Right ())