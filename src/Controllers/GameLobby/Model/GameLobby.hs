module Controllers.GameLobby.Model.GameLobby
  ( GameLobby (GameLobby),
    ClientLobbyJoinResult (ClientLobbyJoinResult, broadcastChannel),
    gameStarted,
    addPlayer,
    pendingGame,
    lobbyPlayers,
    awaiting,
    channel,
    openedAt,
    playerIdGenerator,
    duplicateBroadcastChannel,
    inLobby,
    lobbyIsFull,
    gameLanguage
  )
where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Controllers.Common.CacheableSharedResource
import qualified Controllers.Game.Model.ServerGame as S
import qualified Controllers.Game.Model.ServerPlayer as SP
import Controllers.GameLobby.Api
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import System.Random
import Wordify.Rules.Game
import Prelude

data ClientLobbyJoinResult = ClientLobbyJoinResult
  { -- For listening and sending events to the game lobby from the client
    broadcastChannel :: TChan LobbyMessage,
    -- If the lobby was made full as a result of the client join, the newly created server game
    createdGame :: Maybe S.ServerGame,
    previouslyJoined :: Bool
  }

data GameLobby = GameLobby
  { pendingGame :: Game,
    lobbyPlayers :: TVar [SP.ServerPlayer],
    awaiting :: Int,
    channel :: TChan LobbyMessage,
    -- TODO: rename this to 'lobbyGenerator'
    playerIdGenerator :: TVar StdGen,
    openedAt :: UTCTime,
    gameLanguage :: T.Text
  }

gameStarted :: ClientLobbyJoinResult -> Bool
gameStarted lobbyJoinResult = isJust $ (createdGame lobbyJoinResult)

duplicateBroadcastChannel :: GameLobby -> STM (TChan LobbyMessage)
duplicateBroadcastChannel gameLobby = dupTChan . channel $ gameLobby

addPlayer :: GameLobby -> SP.ServerPlayer -> STM Bool
addPlayer lobby newPlayer = do
  currentPlayers <- readTVar (lobbyPlayers lobby)

  if ((not $ playerAlreadyExists currentPlayers (SP.playerId newPlayer)))
    then do
      let newPlayers = currentPlayers ++ [newPlayer]
      writeTVar (lobbyPlayers lobby) newPlayers
      return True
    else return False
  where
    playerAlreadyExists :: [SP.ServerPlayer] -> T.Text -> Bool
    playerAlreadyExists serverPlayers id = isJust $ find (\x -> SP.playerId x == id) serverPlayers

lobbyIsFull :: GameLobby -> STM Bool
lobbyIsFull lobby = do
  players <- readTVar (lobbyPlayers lobby)
  pure $ length players == awaiting lobby

inLobby :: GameLobby -> T.Text -> STM Bool
inLobby lobby playerIdentifier = isJust . find isPlayer <$> readTVar (lobbyPlayers lobby)
  where
    isPlayer player = (SP.playerId player) == playerIdentifier
