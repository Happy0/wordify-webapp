module Modules.Games.Api
  ( GameService,
    makeGameService,
    getGame,
    peekGame,
  )
where

import ClassyPrelude (Either, IO, Maybe (..), Text, pure, ($))
import Control.Concurrent.STM (STM)
import Controllers.Common.CacheableSharedResource
  ( ResourceCache,
    getCacheableResource,
    makeGlobalResourceCache,
    peekCacheableResource,
  )
import Controllers.Game.Model.ServerGame (ServerGame)
import UnliftIO.Resource (MonadResource, ReleaseKey)

newtype GameService = GameService
  { gameCache :: ResourceCache Text ServerGame
  }

makeGameService :: (Text -> IO (Either Text ServerGame)) -> IO GameService
makeGameService loadGame = do
  cache <- makeGlobalResourceCache loadGame Nothing
  pure (GameService cache)

getGame :: MonadResource m => GameService -> Text -> m (ReleaseKey, Either Text ServerGame)
getGame svc = getCacheableResource (gameCache svc)

peekGame :: GameService -> Text -> STM (Maybe ServerGame)
peekGame svc = peekCacheableResource (gameCache svc)
