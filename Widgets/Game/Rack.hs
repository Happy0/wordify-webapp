module Widgets.Game.Rack (emptyRack, initialiseRack) where

    import Wordify.Rules.Tile
    import Import
    import qualified Data.List as L

    -- Todo: Make empty
    emptyRack :: Widget
    emptyRack = initialiseRack $ (Letter 'B' 2) : (take 5 $ repeat (Letter 'A' 5))

    initialiseRack :: [Tile] -> Widget
    initialiseRack tiles =
        do
            addScriptRemote "http://code.jquery.com/jquery-1.10.2.js"
            addScriptRemote "http://code.jquery.com/ui/1.11.4/jquery-ui.js"

            [whamlet|
                <div #rack-container>
                    <div #rack>
                        $forall tile <- tiles
                            <span .slot> ^{templateTile tile}

                        $forall slot <- [0 , emptySlots]
                            <span .slot>
            |]
            toWidget
                [cassius|
                    #rack-container
                        left: 25%
                        position: absolute;

                    #rack
                        width: #{rackLength}
                        height: #{rackHeight}
                        background-color: brown
                        float: left
                    .slot
                        float: left
                        margin-right: 2px
                        width: 32px
                        height: 32px
                |]
            toWidget
                [julius|

                    $(".tile").draggable({ snap: ".square,.slot",
                        revert: function (isValid) {
                        // If the user drops the tile on a spot that is not droppable, we return the tile to the rack
                            if (!isValid) {
                                dropOnEmptySlot($(this));
                                return true;
                            }
                        }     
                    });
                    $(".tile").disableSelection();

                    var makeEmptySlotsDroppable = function() {
                        $(".slot").not(":has(.tile)").droppable(
                            {accept:".tile",
                            drop: function( event, ui ) {
                              // When dropped, the element is not attached to the DOM element. Instead, its position is changed relative to where it
                              // was originally. We manually attach it to the DOM element. When subsequently dragged, it seems to go under the board,
                              // so we set a z-index
                              ui.draggable.detach().appendTo(this);
                              ui.draggable.attr("style", "position: relative; left: 0px; top: 0px; z-index: 10;");
                              makeEmptySlotsDroppable();

                              $(this).droppable("disable");
                            }});

                        $(".slot").not(":has(.tile)").droppable("enable");
                    }

                    /**
                     * Drops the given tile on the first available empty spot
                    */
                    var dropOnEmptySlot = function(tile) {
                        var emptyTarget = $(".slot").not(":has(.tile)").first();
                        tile.attr("style", "position: relative; left: 0px; top: 0px; z-index: 10;");
                        tile.appendTo(emptyTarget);
                    }

                    makeEmptySlotsDroppable();
                |]
        where
            rackLength = show (32 * 8 :: Int)
            rackHeight = show (32 + 10:: Int)
            tilesOnRack = length tiles
            emptySlots = ((7 - tilesOnRack) + 1)
            headTile = L.head tiles

    templateTile tile = $(widgetFile "tile")

