module Controllers.Game.Model.ServerPlayer (ServerPlayer(ServerPlayer)) where

	import Data.Text
	import Prelude

	data ServerPlayer = ServerPlayer {playerNumber :: Int, identifier :: Text}