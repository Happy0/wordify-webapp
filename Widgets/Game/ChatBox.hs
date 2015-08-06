module Widgets.Game.ChatBox (chatWidget) where

    import Import

    chatWidget :: Widget
    chatWidget =
        do
            [whamlet|
                <div>
                    <ol .chat-box>

                    <input .chat-input-box>

            |]
            toWidget
                [cassius|
                  li
                    list-style: none

                  .chat-box
                    border: 1px solid
                    height: 350px
                    width: 300px
                    overflow: scroll

                  .chat-input-box
                    width: 300px

                  
                |]