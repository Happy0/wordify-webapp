module Controllers.Game.Persist (getLobbyPlayer, persistNewLobby, persistNewLobbyPlayer, deleteLobby, getLobby) where

import ClassyPrelude (Either (Left, Right), IO, Maybe (Just, Nothing), Word64, error, flip, fromMaybe, fst, id, liftIO, maybe, otherwise, show, snd, ($), (+), (++), (.), (==), String, Bool)
import Control.Applicative
import Control.Concurrent.STM
import Control.Error.Util
import Control.Monad
import Control.Monad.Except
import Controllers.Game.Model.ServerPlayer
import Controllers.GameLobby.Model.GameLobby
import Controllers.User.Model.ServerUser (ServerUser (ServerUser))
import Controllers.User.UserController (UserController)
import qualified Controllers.User.UserController as UC
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as Mp
import Data.Pool
import qualified Data.Text as T
import Data.Time.Clock
import Database.Persist.Sql
import Foundation
import qualified Model as M
import System.Random
import System.Random.Internal
import System.Random.SplitMix
import Text.Read (read)
import Wordify.Rules.Game (History (..), history, bag, makeGame)
import Wordify.Rules.LetterBag
import Wordify.Rules.Tile
import qualified Data.List.Split as SL
import Model.GameSetup (LocalisedGameSetup(..))

deleteLobby :: App -> T.Text -> IO ()
deleteLobby app gameId = do
  withPool (appConnPool app) $ do
    deleteWhere [M.InviteNotificationLobbyId ==. M.LobbyKey gameId]
    deleteWhere [M.LobbyInviteLobby ==. M.LobbyKey gameId]
    deleteWhere [M.LobbyPlayerGame ==. gameId]
    deleteWhere [M.LobbyGameId ==. gameId]

getLobby :: ConnectionPool -> UserController -> LocalisedGameSetups -> T.Text -> IO (Either T.Text GameLobby)
getLobby pool userCtrl localisedGameSetups gameId = do
  dbEntries <- withPool pool $ do
    maybeLobby <- selectFirst [M.LobbyGameId ==. gameId] []
    case maybeLobby of
      Nothing -> return $ Left (T.concat ["Game with id ", gameId, " does not exist"])
      Just lobbyModel -> do
        players <- selectList [M.LobbyPlayerGame ==. gameId] []
        return $ Right (lobbyModel, players)

  case dbEntries of
    Right (Entity _ (M.Lobby gameId originalLetterBag letterBagSeed maybeLocale numPlayers createdAt), playerModels) -> do
      players <- mapM (playerFromLobbyEntity userCtrl) playerModels
      serverPlayers <- newTVarIO players
      channel <- newBroadcastTChanIO
      playerIdGenerator <- getStdGen
      playerIdGeneratorTvar <- newTVarIO playerIdGenerator
      runExceptT $ do
        let locale = fromMaybe "en" maybeLocale
        -- This could be more efficient than individual fetches, but it doesn't matter for now
        let maybeLocalisedSetup = Mp.lookup locale localisedGameSetups
        setup@(GameSetup _ dictionary bag _ _) <- hoistEither (note "Locale invalid" maybeLocalisedSetup)
        lobbyPlayers <- hoistEither $ makeGameStatePlayers numPlayers
        tiles <- hoistEither $ dbTileRepresentationToTiles bag originalLetterBag
        let bag = makeBagUsingGenerator tiles (stdGenFromText letterBagSeed :: StdGen)
        pendingGame <- hoistEither $ mapLeft (T.pack . show) (makeGame lobbyPlayers bag dictionary)
        return $ GameLobby pendingGame serverPlayers numPlayers channel playerIdGeneratorTvar createdAt locale setup
    Left err -> return $ Left err

getLobbyPlayer :: ConnectionPool -> UserController -> T.Text -> T.Text -> IO (Maybe ServerPlayer)
getLobbyPlayer pool userCtrl gameId playerId = do
  playerEntity <- withPool pool $ selectFirst [M.LobbyPlayerGame ==. gameId, M.LobbyPlayerPlayerId ==. playerId] []

  case playerEntity of
    Nothing -> pure Nothing
    Just entity -> Just <$> playerFromLobbyEntity userCtrl entity


persistNewLobby :: Pool SqlBackend -> T.Text -> T.Text -> GameLobby -> IO ()
persistNewLobby pool gameId locale gameLobby = do
  let History letterBag _ = history (pendingGame gameLobby)
  let game = pendingGame gameLobby

  withPool pool $ do
    _ <-
      insert $
        M.Lobby
          gameId
          (tilesToDbRepresentation (tiles letterBag))
          (stdGenToText (getGenerator (bag game)))
          (Just locale)
          (awaiting gameLobby)
          (openedAt gameLobby)

    return ()

persistNewLobbyPlayer :: Pool SqlBackend -> T.Text -> ServerPlayer -> IO ()
persistNewLobbyPlayer pool gameId serverPlayer = withPool pool $ persistLobbyPlayers gameId [serverPlayer]

withPool = flip runSqlPersistMPool

persistLobbyPlayers gameId players =
  let playersWithNumbers = L.zip [1 .. 4] players
   in forM_ playersWithNumbers $ do
        \(playerNumber, player) ->
          insert $
            M.LobbyPlayer gameId (playerId player) playerNumber (lastActive player)

tilesToDbRepresentation :: [Tile] -> T.Text
tilesToDbRepresentation tiles = T.pack (L.intercalate "," (L.map tileToDbRepresentation tiles))

tileToDbRepresentation :: Tile -> String
tileToDbRepresentation (Letter lettr val) = lettr
tileToDbRepresentation (Blank Nothing) = "_"
tileToDbRepresentation (Blank (Just letter)) = L.map C.toLower letter

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft func (Left err) = Left $ func err
mapLeft _ (Right r) = Right r

playerFromLobbyEntity :: UserController -> Entity M.LobbyPlayer -> IO ServerPlayer
playerFromLobbyEntity userCtrl (Entity _ (M.LobbyPlayer gameId playerId _ lastActive)) =
  do
    maybeUser <- UC.getUser userCtrl playerId
    case maybeUser of
      -- User was deleted from database?
      Nothing -> return $ makeNewPlayer (ServerUser playerId Nothing) gameId 0 Nothing
      Just serverUser -> return (makeNewPlayer serverUser gameId 0 lastActive)

dbTileRepresentationToTiles :: LetterBag -> T.Text -> Either T.Text [Tile]
dbTileRepresentationToTiles letterBag textRepresentation =
  mapM getTile (SL.splitOn "," (T.unpack textRepresentation))
  where
    letterMap = validLetters letterBag
    getTile :: String -> Either T.Text Tile
    getTile stringRepresentation
      | L.all C.isLower stringRepresentation = Right $ Blank (Just $ L.map C.toUpper stringRepresentation)
      | stringRepresentation == "_" = Right $ Blank Nothing
      | otherwise = case Mp.lookup stringRepresentation letterMap of
          Just tile -> Right tile
          _ -> Left $ T.pack $ stringRepresentation ++ " not found in letterbag"

stdGenToText :: StdGen -> T.Text
stdGenToText stdGen =
  let (a, b) = toSeedStdGen stdGen
   in T.concat [T.pack (show a), " ", T.pack (show b)]

stdGenFromText :: T.Text -> StdGen
stdGenFromText stdGenText =
  let [a, b] = T.split (== ' ') stdGenText
   in fromSeedStdGen (read (T.unpack a), read (T.unpack b))

toSeedStdGen :: StdGen -> (Word64, Word64)
toSeedStdGen = unseedSMGen . unStdGen

fromSeedStdGen :: (Word64, Word64) -> StdGen
fromSeedStdGen = StdGen . seedSMGen'
