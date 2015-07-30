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
                <table .move_list_table>
                    $forall move <- moves
                        <tr>
                            $case move
                                $of Scored formed
                                    <td>^{templateScoreMove formed}
                                    <td .move_list_cell .overall_score>#{overallScore formed}
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
                <table .move_list_move_table>
                    $forall word <- allWordsFormed
                        <tr>
                            <td .move_list_cell>#{prettyPrintIntersections placed word}
                            <td .move_list_cell>#{scoreWord placed word}

            |]
        where
            allWordsFormed = allWords formed
            placed = playerPlacedMap formed

    moveCss :: Widget
    moveCss = 
        do
            toWidget
                [cassius| 
                    .move_list_table
                        border-collapse: collapse
                        border: 2px solid black

                    .move_list_move_table
                        border-collapse: collapse
                        border: 2px solid black
                        width: 100%

                    .overall_score
                        font-weight: bold

                    .move_list_cell
                        border: 1px
                        border-style: solid double
                        background-color: #C8A684
                |]