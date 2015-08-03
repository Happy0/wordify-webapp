module Widgets.Game.ChatBox (chatWidget) where

    import Import

    chatWidget :: Widget
    chatWidget =
        do
            [whamlet|
                <div class="chat_box">
                    <ol>
                        <li>baws

            |]
            toWidget
                [cassius|
                  li
                    list-style: none

                  .chat_box
                    height: 300px
                    width: 150px 

                  
                |]