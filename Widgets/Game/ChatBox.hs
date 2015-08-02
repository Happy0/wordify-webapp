module Widgets.Game.ChatBox (chatWidget) where

    import Import

    chatWidget :: Widget
    chatWidget =
        do
            [whamlet|
                <ol>

            |]
            toWidget
                [cassius|
                  li
                    list-style: none

                  
                |]