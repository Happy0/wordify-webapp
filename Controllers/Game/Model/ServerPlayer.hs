module Controllers.Game.Model.ServerPlayer (ServerPlayer(ServerPlayer), name, makeNewPlayer, makeNewPlayerId) where

	import Data.Text
	import Prelude
	import Network.Mail.Mime
	import Control.Applicative
	import System.Random

	data ServerPlayer = ServerPlayer {name :: Text, identifier :: Text}

	makeNewPlayer :: Text -> Text -> ServerPlayer
	makeNewPlayer playerName gameId = ServerPlayer playerName gameId

	makeNewPlayerId :: IO Text
	makeNewPlayerId = pack . fst . randomString 8 <$> getStdGen
