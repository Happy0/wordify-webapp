module Widgets.Game.Rack (initialiseRack) where

    import Wordify.Rules.Tile
    import Import

    initialiseRack :: [Tile] -> Widget
    initialiseRack tiles =
        do
            addScriptRemote "http://code.jquery.com/jquery-1.10.2.js"
            addScriptRemote "http://code.jquery.com/ui/1.11.4/jquery-ui.js"

            [whamlet|
                    <div id="rack" style="width:#{rackLength}px;height:#{rackHeight}px;position:relative;">
                        $forall tile <- tiles
                            <span class="slot"> ^{templateTile tile}

                        $forall slot <- [0 , emptySlots]
                            <span style="float:left"> baws
            |]
            toWidget
                [cassius|
                    #rack
                        list-style-type: none
                    .slot
                        float: left
                        margin-right: 2px
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

                $(".slot").draggable({ snap: ".square", revert : sendBackToSlot });
                $(".slot").disableSelection();
                |]
        where
            rackLength = show (32 * 7 :: Int)
            rackHeight = show (32 :: Int)
            tilesOnRack = length tiles
            emptySlots = 7 - tilesOnRack

    templateTile tile = $(widgetFile "tile")

