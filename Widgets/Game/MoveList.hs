module Widgets.Game.MoveList (MoveSummary(Passed, Exchanged, Scored), movesWidget) where

    import Wordify.Rules.FormedWord
    import Import
    import Wordify.Rules.Move

    data MoveSummary = Passed | Exchanged | Scored FormedWords | Finished

    movesWidget :: [GameTransition] -> Widget
    movesWidget moves = createMoveList $ map toSummary moves
        where
            toSummary move =
                case move of
                    ExchangeTransition _ _ _ -> Exchanged
                    PassTransition _ -> Passed
                    GameFinished _ _ _ -> Finished
                    MoveTransition _ _ formed -> Scored formed

    createMoveList :: [MoveSummary] -> Widget
    createMoveList moves =
        do
            moveCss
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
                                $of Finished
                                    <td> Finished!
            |]

    templateScoreMove :: FormedWords -> Widget
    templateScoreMove formed =
        do
            [whamlet|
                <table>
                    $forall word <- allWordsFormed
                        <tr>
                            <td>#{prettyPrintIntersections placed word}
                            <td>#{scoreWord placed word}

            |]
        where
            allWordsFormed = allWords formed
            placed = playerPlacedMap formed

    moveCss :: Widget
    moveCss = 
        do
            toWidget
                [cassius| 
                    table, th, td
                        border: 1px solid black
                |]