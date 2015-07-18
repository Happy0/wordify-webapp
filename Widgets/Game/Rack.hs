module Widgets.Game.Rack (emptyRack, initialiseRack) where

    import Wordify.Rules.Tile
    import Import

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

                    // Todo: Try to find some way to make the tiles on the rack re-arrangable
                    $(".tile").draggable({ snap: ".square,.slot", revert: "invalid"});
                    $(".tile").disableSelection();

                    $(".slot").not(":has(.tile)").droppable(
                        {accept:".tile",
                        drop: function( event, ui ) {
                          // When dropped, the element is not attached to the DOM element. Instead, its position is changed relative to where it
                          // was originally. We manually attach it to the DOM element. When subsequently dragged, it seems to go under the board,
                          // so we set a z-index
                          ui.draggable.detach().appendTo(this);
                          ui.draggable.attr("style", "position: relative; left: 0px; top: 0px; z-index: 10;");
                          $(this).droppable("disable");
                        }});



                |]
        where
            rackLength = show (32 * 8 :: Int)
            rackHeight = show (32 + 10:: Int)
            tilesOnRack = length tiles
            emptySlots = 7 - tilesOnRack

    templateTile tile = $(widgetFile "tile")

