module Controllers.Game.Api (ClientMessage(ChatMessage), GameMessage, ServerResponse(PlayerSaid)) where

    import Data.Aeson
    import Data.Aeson.Types
    import Control.Applicative
    import Data.Text
    import Prelude
    import qualified Data.HashMap.Strict as HM
    import Model.Api
    import Wordify.Rules.Board
    import Wordify.Rules.Tile
    import Wordify.Rules.Square

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

    instance ToJSON Board where
        toJSON board = undefined

    instance ToJSON Square where
        toJSON (Normal tile) = object ["tile" .= tile]
        toJSON (DoubleLetter tile) = undefined
        toJSON (TripleLetter tile) = undefined
        toJSON (DoubleWord tile) = undefined
        toJSON (TripleWord tile) = undefined

    instance ToJSON Tile where
        toJSON (Letter letter value) = object ["letter" .= letter, "value" .= value]
        toJSON (Blank (Just letter)) = object ["letter" .= letter, "value" .= (0 :: Int)]
        toJSON _ = object ["letter" .= '_', "value" .= (0 :: Int)]
