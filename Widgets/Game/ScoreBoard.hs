module Widgets.Game.ScoreBoard (scoreWidget) where

    import Wordify.Rules.FormedWord
    import Import
    import Wordify.Rules.Move
    import Wordify.Rules.Player
    import Controllers.Game.Model.MoveSummary

    scoreWidget :: [Player] -> [MoveSummary] -> Widget
    scoreWidget players moves = 
        do
            createOverallScoreList players
            createMoveList moves
            toWidget
                [cassius|
                    .move-list
                        height: 300px
                        overflow-y: scroll
                        float: left
                        background-color: #C8A684

                    .move-list-table
                        width: 300px
                        border-collapse: collapse
                        border: 2px solid black

                    .move-list-move-table
                        border-collapse: collapse
                        border: 2px solid black
                        width: 100%

                    .overall-score
                        font-weight: bold

                    .move-list-cell
                        border: 1px
                        border-style: solid double
                        background-color: #C8A684
                        height: 20px
                |]

    createOverallScoreList :: [Player] -> Widget
    createOverallScoreList players = 
        do
            [whamlet|
                <table .move-list-table>
                    $forall player <- players
                        <tr>
                            <td .move-list-cell> #{name player}
                            <td .move-list-cell> #{score player}


                    $forall emptySlot <- emptySlots
                        <tr>
                            <td .move-list-cell>
                            <td .move-list-cell>
            |]
            where
                emptySlots = [1 .. 4 - (length players)]

    createMoveList :: [MoveSummary] -> Widget
    createMoveList moves =
        do
            [whamlet|
                <div .move-list> 
                    <table .move-list-table>
                        $forall move <- moves
                            <tr>
                                $case move
                                    $of Scored playerName formed
                                        <td .move-list-cell> #{playerName}
                                        <td>^{templateScoreMove formed}
                                        <td .move-list-cell .overall-score>#{overallScore formed}
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
                <table .move-list-move-table>
                    $forall word <- allWordsFormed
                        <tr>
                            <td .move-list-cell>#{prettyPrintIntersections placed word}
                            <td .move-list-cell>#{scoreWord placed word}

            |]
        where
            allWordsFormed = allWords formed
            placed = playerPlacedMap formed