module Controllers.GameLobby.GameLobby (handleChannelMessage, handleClientMessage) where

	import Prelude
	import Foundation
	import Control.Concurrent.STM.TVar
	import Controllers.GameLobby.Api
	import Controllers.Game.Model.ServerPlayer
	import Controllers.GameLobby.Api
	import Model.Api
	import Controllers.Game.Model.GameLobby
	import qualified Data.Text as T
	import Control.Concurrent.STM.TChan
	import Control.Monad
	import Control.Monad.STM
	import Control.Concurrent.STM.TVar

	handleChannelMessage :: LobbyMessage -> LobbyResponse
	handleChannelMessage (PlayerJoined serverPlayer) = Joined serverPlayer

	handleClientMessage :: App -> T.Text -> ClientRequest -> IO (Either T.Text LobbyResponse)
	handleClientMessage app gameId (Join Nothing) = undefined
	handleClientMessage app gameId (Join (Just textId)) = undefined

	handleJoinNewPlayer :: App -> T.Text -> TVar GameLobby -> IO LobbyResponse
	handleJoinNewPlayer app gameId gameLobby =
		do
			playerId <- makeNewPlayerId
			atomically $
				do
					lobby <- readTVar gameLobby
					let players = lobbyPlayers lobby
					let newPlayerName =  T.concat ["player", T.pack . show $ length players]
					let newPlayer = makeNewPlayer newPlayerName playerId
					let newPlayers = players ++ [newPlayer]

					modifyTVar gameLobby $ updatePlayers newPlayers
					
					when (length newPlayers == (awaiting lobby)) $
						do
							removeGameLobby app gameId
							createGame app gameId lobby

							-- Broadcast that the game is ready to begin
							let broadcastChannel = channel lobby
							writeTChan broadcastChannel (PlayerJoined newPlayer)
							writeTChan broadcastChannel LobbyFull

					return JoinSuccess

	removeGameLobby :: App -> T.Text -> STM ()
	removeGameLobby app gameId = undefined

	createGame :: App -> T.Text -> GameLobby -> STM ()
	createGame app gameId lobby = undefined

	