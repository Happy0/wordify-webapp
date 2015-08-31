module Controllers.GameLobby.Api(LobbyMessage(PlayerJoined, LobbyFull), LobbyResponse(Joined, JoinSuccess, StartGame, GameAlreadyStarted, GameDoesNotExist, InvalidPlayerID)) where

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

    {-
        Messages sent over the lobby's broadcast channel.
    -}
    data LobbyMessage = PlayerJoined ServerPlayer | LobbyFull Text

    {-
        Messages sent to clients via their websocket connection.
    -}
    data LobbyResponse = Joined ServerPlayer | JoinSuccess Text (Maybe Text) | StartGame Text | GameAlreadyStarted | GameDoesNotExist | InvalidPlayerID

    instance ToJSON LobbyResponse where
        toJSON (Joined player) = object ["name" .= name player]
        toJSON (StartGame gameId) = object ["gameId" .= gameId]
        toJSON (JoinSuccess gameId newId) = object $ maybe [] (\playerId -> ["id" .= newId, "gameId" .= gameId]) newId
        toJSON GameAlreadyStarted = object []
        toJSON GameDoesNotExist = object []
        toJSON InvalidPlayerID = object []

    instance ServerMessage LobbyResponse where
        commandName (Joined _) = "joined"
        commandName (JoinSuccess _ _) = "joinSuccess"
        commandName (StartGame _) = "startGame"
        commandName GameAlreadyStarted = "alreadyStarted"
        commandName GameDoesNotExist = "invalidGameId"
        commandName InvalidPlayerID = "InvalidPlayerId"
