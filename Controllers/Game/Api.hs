module Controllers.Game.Api (ClientMessage(ChatMessage, BoardMove),
                             GameMessage(PlayerBoardMove),
                             ServerResponse(PlayerSaid, BoardMoveSuccess, InvalidCommand)) where

    import Data.Aeson
    import Data.Aeson.Types
    import qualified Data.Vector as V
    import Control.Applicative
    import Data.Text
    import Prelude
    import qualified Data.HashMap.Strict as HM
    import Model.Api
    import Wordify.Rules.Board
    import Wordify.Rules.Player
    import Wordify.Rules.Pos
    import Wordify.Rules.Tile
    import Wordify.Rules.Square
    import qualified Data.List as L

    data ClientMessage = ChatMessage Text | BoardMove [(Pos, Tile)]

    -- Response messages to a client request over the websocket
    data ServerResponse = PlayerSaid Text Text |
                          BoardMoveSuccess [Tile] |
                          InvalidCommand Text

    -- Messages sent over the server game channel to notify clients of changes
    -- such as a player making a move successfully
    data GameMessage = PlayerBoardMove [(Pos, Tile)]

    instance ServerMessage GameMessage where
        commandName (PlayerBoardMove _) = "playerBoardMove"

    instance ToJSON GameMessage where
        toJSON (PlayerBoardMove placed) = object ["placed" .= fmap writePosAndTile placed]

    instance FromJSON ClientMessage where
        parseJSON (Object request) =
            case HM.lookup "command" request of
                Just (String command) ->
                    request .: "payload" >>= parseCommand command
                _ -> error "Expected command to have text value"

        parseJSON _ = error "Invalid JSON"

    instance ToJSON ServerResponse where
        toJSON (PlayerSaid name message) = object ["name" .= name, "message" .= message]
        toJSON (BoardMoveSuccess tiles) = object ["rack" .= toJSON tiles]
        toJSON (InvalidCommand msg) = object ["error" .= msg]

    instance ServerMessage ServerResponse where
        commandName (PlayerSaid _ _) = "said"
        commandName (BoardMoveSuccess _) = "boardMoveSuccess"
        commandName (InvalidCommand _) = "error"

    writePosAndTile :: (Pos, Tile) -> Value
    writePosAndTile (pos, tile) = object ["pos" .= toJSON pos, "tile" .= toJSON tile]

    parseCommand :: Text -> Value -> Parser ClientMessage
    parseCommand "say" value = parseChatMessage value
    parseCommand "boardMove" value = parseBoardMove value
    parseCommand _ _ = error "Unrecognised command"

    parseChatMessage :: Value -> Parser ClientMessage
    parseChatMessage (Object object) = ChatMessage <$> object .: "message"
    parseChatMessage _ = error "Unrecognised chat message"

    parseBoardMove :: Value -> Parser ClientMessage
    parseBoardMove (Array a) = BoardMove <$> (sequence . V.toList $ fmap getPosAndTile a)
    parseBoardMove _ = error "A board move should have an array as its payload"

    getPosAndTile :: Value -> Parser (Pos, Tile)
    getPosAndTile (Object val) =
        do
            pos <-  val .: "pos" >>= parseJSON
            tile <-  val .: "tile" >>= parseJSON
            return (pos, tile)
    getPosAndTile _ = error "expected object payload with pos and tile"

    instance ToJSON Board where
        toJSON = toJSON . groupSquaresByColumn . allSquares

    instance ToJSON Square where
        toJSON (Normal tile) = object ["tile" .= tile, "bonus" .= ("N" :: Text)]
        toJSON (DoubleLetter tile) = object ["tile" .= tile, "bonus" .= ("DL" :: Text)]
        toJSON (TripleLetter tile) = object ["tile" .= tile, "bonus" .= ("TL" :: Text)]
        toJSON (DoubleWord tile) = object ["tile" .= tile, "bonus" .= ("DW" :: Text)]
        toJSON (TripleWord tile) = object ["tile" .= tile, "bonus" .= ("TW" :: Text)]

    instance ToJSON Player where
        toJSON player = object ["name" .= name player, "score" .= score player]

    instance ToJSON Tile where
        toJSON (Letter letter value) = object ["letter" .= letter, "value" .= value]
        toJSON (Blank (Just letter)) = object ["letter" .= letter, "value" .= (0 :: Int)]
        toJSON _ = object ["letter" .= '_', "value" .= (0 :: Int)]

    instance FromJSON Pos where
        parseJSON (Object value) = do
            x <- value .: "x"
            y <- value .:"y"
            let pos = posAt(x,y)
            case pos of
                Nothing -> error "Position is not in valid board coordinate boundaries"
                Just p -> return p
        parseJSON _ = error "Position must be a JSON object value, rather than an array, etc"

    instance ToJSON Pos where
        toJSON pos = object ["x" .= xPos pos, "y" .= yPos pos]

    instance FromJSON Tile where
        parseJSON (Object value) = do
            letter <- value .: "letter"
            tileValue <- value .: "value"

            return $
                 case tileValue of
                        0 -> case letter of
                                '_' -> Blank Nothing
                                chr -> Blank (Just chr)
                        x -> Letter letter tileValue
        parseJSON _ = error "Tile must be a JSON object value, rather than an array, etc"

    groupSquaresByColumn :: [(Pos, Square)] -> [[Square]]
    groupSquaresByColumn squares =
        let columns = L.groupBy sameColumn squares
        in (Prelude.map . Prelude.map) snd columns
        where
            sameColumn square1 square2 = xPos (fst square1) == xPos (fst square2)


