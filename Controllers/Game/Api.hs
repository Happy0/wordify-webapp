module Controllers.Game.Api (ClientRequest(CreateGameRequest), ServerResponse(GameCreated)) where

    import Prelude
    import Model.Api
    import Model.ServerGame
    import Control.Monad
    import Data.Aeson
    import Data.Aeson.Types
    import Data.Maybe
    import Data.Text
    import qualified Data.HashMap.Strict as HM



    type NumberPlayers = Int

    data ClientRequest = CreateGameRequest {numberPlayers :: Int, hostNick :: Text}

    data ServerResponse = GameCreated {gameId :: GameId}

    instance ToJSON ServerResponse where
        toJSON (GameCreated gameId) = object ["gameId" .= gameId]

    instance ServerMessage ServerResponse where
        commandName _ = pack "gameCreated"

    instance FromJSON ClientRequest where
        parseJSON (Object request) =
            case HM.lookup "command" request of
                Just (String command) -> 
                    request .: "payload" >>= parseCommand command
                _ -> error "Expected command to have text value"

        parseJSON _ = error "Not a JSON object"

    parseCommand :: Text -> Value -> Parser ClientRequest
    parseCommand "createGame" value = parseCreateGame value

    parseCreateGame :: Value -> Parser ClientRequest
    parseCreateGame (Object object) = CreateGameRequest <$> object .: "players" <*> object .: "nickname"
    parseCreateGame _ = error "Expected JSON object for payload"