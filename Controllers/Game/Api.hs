module Controllers.Game.Api (ClientMessage(ChatMessage), GameMessage) where

    import Data.Aeson
    import Data.Aeson.Types
    import Control.Applicative
    import Data.Text
    import Prelude
    import qualified Data.HashMap.Strict as HM
    import Model.Api

    data ClientMessage = ChatMessage Text

    data ServerResponse = PlayerSaid Text Text

    data GameMessage

    instance FromJSON ClientMessage where
        parseJSON (Object request) =
            case HM.lookup "command" request of
                Just (String command) -> 
                    request .: "payload" >>= parseCommand command
                _ -> error "Expected command to have text value"

        parseJSON _ = error "Invalid JSON"

    instance ToJSON ServerResponse where
        toJSON (PlayerSaid name message) = object ["name" .= name, "message" .= message]

    instance ServerMessage ServerResponse where
        commandName (PlayerSaid _ _) = "said"


    parseCommand :: Text -> Value -> Parser ClientMessage
    parseCommand "say" value = parseChatMessage value
    parseCommand _ _ = error "Unrecognised command"

    parseChatMessage :: Value -> Parser ClientMessage
    parseChatMessage (Object object) = ChatMessage <$> object .: "message" 