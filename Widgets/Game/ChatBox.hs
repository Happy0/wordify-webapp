module Widgets.Game.ChatBox (chatWidget) where

    import Import

    chatWidget :: Widget
    chatWidget =
        do
            [whamlet|
                <div>
                    <ol .chat-box>
                        <li> baws
                        <li> message2

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

                  .chat-input-box
                    width: 300px

                  
                |]