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
                |]
            toWidget
                [julius|
                    var sendBackToSlot = function(event, ui) {
                                    // on older version of jQuery use "draggable"
                                    // $(this).data("draggable")
                                    // on 2.x versions of jQuery use "ui-draggable"
                                    // $(this).data("draggable")
                                    $(this).data("uiDraggable").originalPosition = {
                                        top : 0,
                                        left : 0
                                    };

                                    return !event;
                    };

                    // Todo: Try to find some way to make the tiles on the rack re-arrangable
                    $(".tile").draggable({ snap: ".square", revert : sendBackToSlot});
                    $(".slot").disableSelection();
                |]
        where
            rackLength = show (32 * 8 :: Int)
            rackHeight = show (32 + 10:: Int)
            tilesOnRack = length tiles
            emptySlots = 7 - tilesOnRack

    templateTile tile = $(widgetFile "tile")

