module Handler.Model.ClientGame where

import ClassyPrelude (Text, Maybe (..), Int, Bool, (>), UTCTime, RealFrac (..), map, (*), not, undefined, fmap)
import Data.Aeson
import Controllers.Game.Model.ServerPlayer
import Data.Time.Clock.POSIX
import qualified Wordify.Rules.Player as P
import Wordify.Rules.Tile
import qualified Data.Text as T

instance ToJSON ClientPlayer where
    toJSON (ClientPlayer name score endBonus connected lastSeen) = object [
        "name" .= name, "score" .= score, "endBonus" .= endBonus, "connected" .= connected, "lastSeen" .= lastSeen
        ]

instance ToJSON ClientTile where
    toJSON (ClientLetter letter value) = object ["type" .= T.pack "letter", "letter" .= letter, "value" .= value]
    toJSON (ClientBlank assigned) = object [ "type" .= T.pack "blank", "assigned" .= assigned]

data ClientPlayer = ClientPlayer {name:: Text, score:: Int, endBonus:: Maybe Int, connected :: Bool, lastSeen :: Maybe Int}
data ClientTile = ClientLetter {letter :: Text, value:: Int} | ClientBlank { assigned :: Maybe Text}

fromServerPlayer :: Bool -> ServerPlayer -> P.Player -> ClientPlayer
fromServerPlayer gameOver serverPlayer playerGameState  =
    ClientPlayer (playerName (playerUsername serverPlayer)) (P.score playerGameState) bonus (numConnections serverPlayer > 0) (map toMillisecondsSinceUnixEpoch (lastActive serverPlayer))
    where
        playerName Nothing = ""
        playerName (Just n) = n

        toMillisecondsSinceUnixEpoch :: UTCTime -> Int
        toMillisecondsSinceUnixEpoch utcTime = 1000 * round (utcTimeToPOSIXSeconds utcTime)

        bonus = if not gameOver then Nothing else Just (P.endBonus playerGameState)

fromServerTile :: Tile -> ClientTile
fromServerTile (Blank letter) = ClientBlank (fmap T.pack letter)
fromServerTile (Letter letter value) = ClientLetter (T.pack letter) value 