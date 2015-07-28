module Widgets.Game.MoveList (MoveSummary(Passed, Exchanged, Scored)) where

	import Wordify.Rules.FormedWord

	data MoveSummary = Passed | Exchanged | Scored FormedWords

	createMoveList :: [MoveSummary] -> Widget
	createMoveList moves =
		do
            [whamlet|
                <table>
                	$for move <- moves
                		<p> I'm a move!~
            |]
