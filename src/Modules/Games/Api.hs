module Modules.Games.Api
  ( GameService,
    makeGameService,
    getGame,
    peekGame,
  )
where

import ClassyPrelude (Either (Left, Right), IO, Int, Maybe (..), Text, fmap, pure, zipWith, ($), (.))
import Control.Concurrent.STM (STM, atomically)
import Controllers.Common.CacheableSharedResource
  ( ResourceCache,
    getCacheableResource,
    makeGlobalResourceCache,
    peekCacheableResource,
  )
import Controllers.Game.Model.ServerGame (ServerGame, makeServerGame)
import Controllers.Game.Model.ServerPlayer (makeNewPlayer)
import Repository.GameRepository (AnyGameRepository, GameEntity (..))
import qualified Repository.GameRepository as GR
import UnliftIO.Resource (MonadResource, ReleaseKey)

newtype GameService = GameService
  { gameCache :: ResourceCache Text ServerGame
  }

makeGameService :: AnyGameRepository -> IO GameService
makeGameService repo = do
  cache <- makeGlobalResourceCache (loadGame repo) Nothing
  pure (GameService cache)

loadGame :: AnyGameRepository -> Text -> IO (Either Text ServerGame)
loadGame repo gameId = do
  result <- GR.getGame repo gameId
  case result of
    Left err -> pure (Left err)
    Right entity -> fmap Right $ atomically (gameEntityToServerGame entity)

gameEntityToServerGame :: GameEntity -> STM ServerGame
gameEntityToServerGame entity =
  let serverPlayers = zipWith (\su i -> makeNewPlayer su (gameEntityId entity) 0 Nothing) (gameEntityPlayers entity) [(1 :: Int) ..]
  in makeServerGame
      (gameEntityId entity)
      (gameEntityGame entity)
      serverPlayers
      (gameEntityCreatedAt entity)
      (gameEntityLastMoveMadeAt entity)
      (gameEntityFinishedAt entity)
      (gameEntitySetup entity)

getGame :: MonadResource m => GameService -> Text -> m (ReleaseKey, Either Text ServerGame)
getGame svc = getCacheableResource (gameCache svc)

peekGame :: GameService -> Text -> STM (Maybe ServerGame)
peekGame svc = peekCacheableResource (gameCache svc)
