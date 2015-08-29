module Controllers.Game.Model.ServerPlayer (ServerPlayer(ServerPlayer), name, identifier, makeNewPlayer, makeNewPlayerId) where

	import Data.Text
	import Prelude
	import Network.Mail.Mime
	import Control.Applicative
	import System.Random

	data ServerPlayer = ServerPlayer {name :: Text, identifier :: Text}

	makeNewPlayer :: Text -> Text -> ServerPlayer
	makeNewPlayer playerName gameId = ServerPlayer playerName gameId

	makeNewPlayerId :: StdGen -> (Text, StdGen)
	makeNewPlayerId generator = 
		let (result, newGen) = randomString 8 generator
		in (pack result, newGen)
