module Controllers.Game.Api (ClientMessage(ChatMessage), GameMessage, ServerResponse(PlayerSaid)) where

    import Data.Aeson
    import Data.Aeson.Types
    import Control.Applicative
    import Data.Text
    import Prelude
    import qualified Data.HashMap.Strict as HM
    import Model.Api
    import Wordify.Rules.Board
    import Wordify.Rules.Pos
    import Wordify.Rules.Tile
    import Wordify.Rules.Square
    import qualified Data.List as L

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
    parseChatMessage _ = error "Unrecognised chat message"

    instance ToJSON Board where
        toJSON = toJSON . groupSquaresByColumn . allSquares

    instance ToJSON Square where
        toJSON (Normal tile) = object ["tile" .= tile]
        toJSON (DoubleLetter tile) = object ["tile" .= tile, "bonus" .= ("N" :: Text)]
        toJSON (TripleLetter tile) = object ["tile" .= tile, "bonus" .= ("TL" :: Text)]
        toJSON (DoubleWord tile) = object ["tile" .= tile, "bonus" .= ("DW" :: Text)]
        toJSON (TripleWord tile) = object ["tile" .= tile, "bonus" .= ("TW" :: Text)]

    instance ToJSON Tile where
        toJSON (Letter letter value) = object ["letter" .= letter, "value" .= value]
        toJSON (Blank (Just letter)) = object ["letter" .= letter, "value" .= (0 :: Int)]
        toJSON _ = object ["letter" .= '_', "value" .= (0 :: Int)]

    groupSquaresByColumn :: [(Pos, Square)] -> [[Square]]
    groupSquaresByColumn squares = 
        let columns = L.groupBy sameColumn squares
        in (Prelude.map . Prelude.map) snd columns
        where
            sameColumn square1 square2 = xPos (fst square1) == xPos (fst square2)


