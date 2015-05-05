module Widgets.Board.Board (initialBoard) where

    import Import
    import Wordify.Rules.Board
    import Wordify.Rules.Pos
    import Wordify.Rules.Square
    import Wordify.Rules.Tile
    import Data.List
    import Data.List.Split

    initialBoard :: Widget
    initialBoard = $(widgetFile "board")
        where
            rows = layoutBoard emptyBoard


    templateRow row = [whamlet|
        $forall square <- row
            ^{templateSquare square}
     |]

    templateSquare (pos, square) = 
        [whamlet|
                <div class="square #{squareClass}" style="top:#{xPosition}px; left:#{yPosition}px;">
                    $maybe tile <- tileIfOccupied square
                        ^{templateTile tile}
                    $nothing
                        
        |]

        where
            xPosition = (xPos pos -1) * 32
            yPosition = (yPos pos -1) * 32

            (squareClass, squareText) = case square of
                DoubleLetter(_) -> ("doubleletter" :: Text, "DL" :: Text)
                TripleLetter(_) -> ("tripleletter", "TL")
                DoubleWord(_) -> ("doubleword", "DW")
                TripleWord(_) -> ("tripleword", "TW")
                Normal(_) -> ("normal", "")

    templateTile tile = $(widgetFile "tile")

    {- Layout the board in rows of 15 from (1,1) -> (15,15) -}
    layoutBoard :: Board -> [[(Pos, Square)]]
    layoutBoard = transpose . chunksOf 15 . allSquares