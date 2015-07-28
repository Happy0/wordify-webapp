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
                        <tr>
                            $case move
                                $of Scored formed
                                    <td>^{templateScoreMove formed}
                                    <td>#{overallScore formed}
                                $of Passed
                                    <td>passed
                                $of Exchanged
                                    <td>exchanged
            |]

    templateScoreMove :: FormedWords -> Widget
    templateScoreMove formed =
        do
            [whamlet|
                <table>
                    $forall word <- allWordsFormed
                        <tr>
                            <td>#{prettyPrintIntersections placed word}
                            <td>#(scoreWord word)

            |]
        where
            allWordsFormed = allWords formed
            placed = playerPlacedMap formed