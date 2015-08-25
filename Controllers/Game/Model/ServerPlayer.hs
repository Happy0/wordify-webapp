module Controllers.Game.Model.ServerPlayer (ServerPlayer(ServerPlayer), name) where

	import Data.Text
	import Prelude

	data ServerPlayer = ServerPlayer {name :: Text, identifier :: Text}