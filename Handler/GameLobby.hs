module Handler.GameLobby where

    import Import
    import Yesod.Core
    import Yesod.WebSockets

    getGameLobbyR :: Text -> Handler Html
    getGameLobbyR gameId = defaultLayout $ do
        [whamlet|
            <div .lobby>

        |]
        toWidget
            [julius|

            |]

    lobbyWebSocketHandler :: WebSocketsT Handler ()
    lobbyWebSocketHandler = undefined

    