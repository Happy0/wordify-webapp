module Widgets.Board.Board (initialBoard) where

    import Import
    import Wordify.Rules.Board
    import Wordify.Rules.Pos
    import Wordify.Rules.Square
    import Wordify.Rules.Tile
    import Data.List
    import Data.List.Split

    {- TODO: Style / deal with blank letter tiles -}

    initialBoard :: Widget
    initialBoard = $(widgetFile "board")
        where
            rows = layoutBoard emptyBoard
            width = 32 :: Int
            tileWidth = show $ (width :: Int)
            boardWidth = show $ width * 15

    templateRow row = [whamlet|
        $forall square <- row
            ^{templateSquare square}
     |]

    templateSquare (pos, square) =
        do
            [whamlet|
                    <div class="square #{squareClass}" data-x=#{xPos pos} data-y=#{yPos pos} style="top:#{xPosition}px; left:#{yPosition}px;">
                        $maybe tile <- tileIfOccupied square
                            ^{templateTile tile}
            |]
            toWidget
                [julius|
                    $(".square").droppable({accept: ".tile", 
                        drop: function( event, ui ) {
                          // When dropped, the element is not attached to the DOM element. Instead, its position is changed relative to where it
                          // was originally. We manually attach it to the DOM element. When subsequently dragged, it seems to go under the board,
                          // so we set a z-index
                          ui.draggable.detach().appendTo(this);
                          ui.draggable.attr("style", "position: relative; left: 0px; top: 0px; z-index: 10;");
                        },
                        out: function(event, ui) {

                        }
                    });
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