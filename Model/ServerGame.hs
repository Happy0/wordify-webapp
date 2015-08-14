module Model.ServerGame where

	import Data.Text

	type GameId = Text

	{-
		A game invite lobby is where players accept a game invite by
		pasting a given game URL into their browser and wait for other
		players to join before the game begins
	-}
	data GameInviteLobby = GameInviteLobby GameId

	data ServerGame = ServerGame GameId