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

    data ClientRequest = CreateGameRequest NumberPlayers

    data ServerResponse = GameCreated GameId

    instance ToJSON ServerResponse where
        toJSON (GameCreated gameId) = object ["gameId" .= gameId]

    instance ServerMessage ServerResponse where
        commandName _ = pack "gameCreated"

    instance FromJSON ClientRequest where
        parseJSON (Object request) =
            case HM.lookup "command" request of
                Just (String "createGame") -> return $ CreateGameRequest 4
                _ -> error "Expected command to be text"

        parseJSON _ = error "Not a JSON object"