module Controllers.GameLobby.GameLobby (handleChannelMessage, handleClientMessage) where

	import Prelude
	import Control.Concurrent.STM.TVar
	import Controllers.GameLobby.Api
	import Controllers.Game.Model.ServerPlayer
	import Controllers.GameLobby.Api
	import Model.Api
	import Controllers.Game.Model.GameLobby
	import Data.Text

	handleChannelMessage :: LobbyMessage -> LobbyResponse
	handleChannelMessage msg = undefined

	handleClientMessage :: TVar GameLobby -> ClientRequest -> IO (Either Text LobbyResponse)
	handleClientMessage _ _ = undefined
	