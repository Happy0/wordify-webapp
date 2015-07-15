module Widgets.Game.Rack (initialiseRack) where

    import Wordify.Rules.Tile
    import Import

    initialiseRack :: [Tile] -> Widget
    initialiseRack tiles =
        do
            addScriptRemote "http://code.jquery.com/jquery-1.10.2.js"
            addScriptRemote "http://code.jquery.com/ui/1.11.4/jquery-ui.js"

            [whamlet|
                    <div id="rack" style="width:#{rackLength}px;height:#{rackHeight}px">
                        $forall tile <- tiles
                            <span style="float:left"> ^{templateTile tile}

                        $forall slot <- [0 , emptySlots]
                            <span style="float:left"> baws
            |]
            toWidget
                [julius|
                    $(".tile").draggable({ snap: ".square" });


                |]
        where
            rackLength = show (32 * 7 :: Int)
            rackHeight = show (32 :: Int)
            tilesOnRack = length tiles
            emptySlots = 7 - tilesOnRack

    templateTile tile = $(widgetFile "tile")

