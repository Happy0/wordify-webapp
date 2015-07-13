module Widgets.Game.Rack (initialiseRack) where

    import Wordify.Rules.Tile
    import Import

    initialiseRack :: [Tile] -> Widget
    initialiseRack tiles = 
        [whamlet|
                <div style="width=#{rackLength}px height=#{rackHeight}px">
                    $forall tile <- tiles
                        ^{templateTile tile}
        |]
        where
            rackLength = show (32 * 7 :: Int)
            rackHeight = show (32 :: Int)

    templateTile tile = $(widgetFile "tile")

