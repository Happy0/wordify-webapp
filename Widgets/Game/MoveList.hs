module Widgets.Game.MoveList (MoveSummary(Passed, Exchanged, Scored)) where

    import Wordify.Rules.FormedWord
    import Import

    data MoveSummary = Passed | Exchanged | Scored FormedWords

    createMoveList :: [MoveSummary] -> Widget
    createMoveList moves =
        do
            [whamlet|
                <table>
                    $forall move <- moves
                        <p> I'm a move!~
            |]
