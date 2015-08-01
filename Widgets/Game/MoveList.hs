module Widgets.Game.MoveList (MoveSummary(Passed, Exchanged, Scored), movesWidget) where

    import Wordify.Rules.FormedWord
    import Import
    import Wordify.Rules.Move
    import Wordify.Rules.Player

    data MoveSummary = Passed String | Exchanged String | Scored String FormedWords | Finished

    movesWidget :: [GameTransition] -> Widget
    movesWidget moves = createMoveList $ map toSummary moves
        where
            toSummary move =
                case move of
                    ExchangeTransition _ player _ -> Exchanged $ name player
                    PassTransition _ -> Passed "_"
                    GameFinished _ _ _ -> Finished
                    MoveTransition player _ formed -> Scored (name player) formed

    createMoveList :: [MoveSummary] -> Widget
    createMoveList moves =
        do
            moveCss
            [whamlet|
                <div .move-list> 
                    <table .move_list_table>
                        $forall move <- moves
                            <tr>
                                $case move
                                    $of Scored playerName formed
                                        <td .move_list_cell> #{playerName}
                                        <td>^{templateScoreMove formed}
                                        <td .move_list_cell .overall_score>#{overallScore formed}
                                    $of Passed playerName
                                        <td>passed
                                    $of Exchanged playerName
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
                    .move-list
                        height: 300px
                        overflow-y: scroll
                        float: left

                    .move_list_table
                        width: 300px
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