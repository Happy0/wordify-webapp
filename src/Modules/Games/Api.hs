module Modules.Games.Api
  ( GameService,
    makeGameService,
    getGame,
    peekGame,
    startGame,
    MoveUpdate (..),
    saveMove,
    updatePlayerLastSeen,
  )
where

import ClassyPrelude (Either (Left, Right), IO, Int, Maybe (..), Text, fmap, map, pure, zipWith, ($), (.))
import Controllers.User.Model.ServerUser (ServerUser)
import Data.Time (UTCTime)
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
import Wordify.Rules.Pos (Pos)
import Wordify.Rules.Tile (Tile)
import Data.Functor ((<$>))

data GameService = GameService
  { gameCache :: ResourceCache Text ServerGame
  , gameRepo  :: AnyGameRepository
  }

makeGameService :: AnyGameRepository -> IO GameService
makeGameService repo = do
  cache <- makeGlobalResourceCache (loadGame repo) Nothing
  pure (GameService cache repo)

loadGame :: AnyGameRepository -> Text -> IO (Either Text ServerGame)
loadGame repo gameId = do
  result <- GR.getGame repo gameId
  case result of
    Left err -> pure (Left err)
    Right entity -> Right <$> atomically (gameEntityToServerGame entity)

gameEntityToServerGame :: GameEntity -> STM ServerGame
gameEntityToServerGame entity =
  let serverPlayers = map (\(su, lastActive) -> makeNewPlayer su (gameEntityId entity) 0 lastActive) (gameEntityPlayers entity)
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

startGame :: GameService -> GameEntity -> IO ()
startGame svc entity =
  GR.startNewGame (gameRepo svc) entity

data MoveUpdate
  = BoardMoveUpdate
      { boardMoveGameId    :: Text
      , boardMoveNumber    :: Int
      , boardMovePlaced    :: [(Pos, Tile)]
      , boardMoveBoardText :: Text
      }
  | PassMoveUpdate
      { passMoveGameId    :: Text
      , passMoveNumber    :: Int
      , passMoveBoardText :: Text
      }
  | ExchangeMoveUpdate
      { exchangeMoveGameId    :: Text
      , exchangeMoveNumber    :: Int
      , exchangeMoveTiles     :: [Tile]
      , exchangeMoveBoardText :: Text
      }
  | GameEndMoveUpdate
      { gameEndMoveGameId   :: Text
      , gameEndMoveNumber   :: Int
      , gameEndMovePlaced   :: Maybe [(Pos, Tile)]
      , gameEndMoveBoardText :: Text
      }

saveMove :: GameService -> MoveUpdate -> IO ()
saveMove svc (BoardMoveUpdate gId n placed bText) =
  GR.saveBoardMove (gameRepo svc) gId n placed bText
saveMove svc (PassMoveUpdate gId n bText) =
  GR.savePassMove (gameRepo svc) gId n bText
saveMove svc (ExchangeMoveUpdate gId n tiles bText) =
  GR.saveExchangeMove (gameRepo svc) gId n tiles bText
saveMove svc (GameEndMoveUpdate gId n placed bText) =
  GR.saveGameEnd (gameRepo svc) gId n placed bText

updatePlayerLastSeen :: GameService -> Text -> ServerUser -> UTCTime -> IO ()
updatePlayerLastSeen svc = GR.updatePlayerLastSeen (gameRepo svc)
