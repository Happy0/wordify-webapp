module Controllers.GameLobby.Api(LobbyMessage(PlayerJoined, LobbyFull), LobbyResponse(Joined, JoinSuccess, StartGame), ClientRequest(Join)) where

    import Controllers.Game.Model.ServerPlayer
    import Model.Api
    import Data.Aeson
    import Control.Applicative
    import Data.Maybe
    import qualified Data.HashMap.Strict as HM
    import Data.Text
    import Prelude
    import Data.Aeson
    import Data.Aeson.Types

    data ClientRequest = Join (Maybe Text)

    instance FromJSON ClientRequest where
        parseJSON (Object request) =
            case HM.lookup "command" request of
                Just (String command) -> 
                    request .: "payload" >>= parseCommand command
                _ -> error "Expected command to have text value"

        parseJSON _ = error "Invalid JSON"

    parseCommand :: Text -> Value -> Parser ClientRequest
    parseCommand "join" value = parseJoin value
    parseCommand _  _ = error "Unrecognised command"

    parseJoin :: Value -> Parser ClientRequest
    parseJoin (Object object) = Join <$> object .:? "id"
    parseJoin _ = error "Expected JSON object for payload"

    {-
        Messages sent over the lobby's broadcast channel.
    -}
    data LobbyMessage = PlayerJoined ServerPlayer | LobbyFull

    {-
        Messages sent to clients via their websocket connection.
    -}
    data LobbyResponse = Joined ServerPlayer | JoinSuccess | StartGame

    instance ToJSON LobbyResponse where
        toJSON (Joined player) = object ["name" .= name player]
        toJSON StartGame = object []
        toJSON JoinSuccess = object []

    instance ServerMessage LobbyResponse where
        commandName (Joined _) = "joined"
        commandName JoinSuccess = "joinSuccess"
        commandName StartGame = "startGame"
