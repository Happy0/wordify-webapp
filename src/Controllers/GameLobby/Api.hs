module Controllers.GameLobby.Api
  ( CreateGameLobby (CreateGameLobby),
    LobbyMessage (PlayerJoined, LobbyFull),
    LobbyResponse (Joined, JoinSuccess, StartGame),
    LobbyInputError (GameLobbyDoesNotExist, InvalidPlayerID, LobbyAlreadyFull),
  )
where

import Control.Applicative
import Control.Monad
import Controllers.Game.Model.ServerPlayer
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text
import Model.Api
import Prelude

data CreateGameLobby = CreateGameLobby {numPlayers :: Int, locale :: Text}

instance FromJSON CreateGameLobby where
  parseJSON (Object o) = CreateGameLobby <$> o .: "num_players" <*> o .: "locale"
  parseJSON _ = mzero

{-
    Messages sent over the lobby's broadcast channel.
-}
data LobbyMessage = PlayerJoined ServerPlayer | LobbyFull Text

{-
    Messages sent to clients via their websocket connection.
-}
data LobbyResponse = Joined ServerPlayer | JoinSuccess Text Text | StartGame Text

{-
    When the client gives invalid input when trying to join a lobby
-}
data LobbyInputError = GameLobbyDoesNotExist | InvalidPlayerID | LobbyAlreadyFull

instance ToJSON LobbyResponse where
  toJSON (Joined player) = object ["name" .= name player]
  toJSON (StartGame gameId) = object ["gameId" .= gameId]
  toJSON (JoinSuccess gameId newId) = object ["gameId" .= gameId, "id" .= newId]

instance ServerMessage LobbyResponse where
  commandName (Joined _) = "joined"
  commandName (JoinSuccess _ _) = "joinSuccess"
  commandName (StartGame _) = "startGame"
