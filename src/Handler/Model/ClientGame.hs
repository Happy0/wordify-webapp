module Handler.Model.ClientGame where

import ClassyPrelude (Text, Maybe (..), Int, Bool, (>), UTCTime, RealFrac (..), map, (*), not)
import Data.Aeson
import Controllers.Game.Model.ServerPlayer
import Data.Time.Clock.POSIX
import qualified Wordify.Rules.Player as P

instance ToJSON ClientPlayer where
    toJSON (ClientPlayer name score endBonus connected lastSeen) = object [
        "name" .= name, "score" .= score, "endBonus" .= endBonus, "connected" .= connected, "lastSeen" .= lastSeen
        ]

data ClientPlayer = ClientPlayer {name:: Text, score:: Int, endBonus:: Maybe Int, connected :: Bool, lastSeen :: Maybe Int}

fromServerPlayer :: Bool -> ServerPlayer -> P.Player -> ClientPlayer
fromServerPlayer gameOver (ServerPlayer name _ _ numConnections lastActive) playerGameState  =
    ClientPlayer (playerName name) (P.score playerGameState) bonus (numConnections > 0) (map toMillisecondsSinceUnixEpoch lastActive)
    where
        playerName Nothing = ""
        playerName (Just n) = n

        toMillisecondsSinceUnixEpoch :: UTCTime -> Int
        toMillisecondsSinceUnixEpoch utcTime = 1000 * round (utcTimeToPOSIXSeconds utcTime)

        bonus = if not gameOver then Nothing else Just (P.endBonus playerGameState)
