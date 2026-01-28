module Handler.Model.ClientGame where

import ClassyPrelude (Text, Maybe (..), Int, Bool, (>), const, UTCTime (UTCTime), RealFrac (..), (.), map, undefined, (*))
import Data.Aeson
import Controllers.Game.Model.ServerPlayer
import Data.Time.Clock.POSIX

instance ToJSON ClientPlayer where
    toJSON (ClientPlayer name score endBonus connected lastSeen) = object [
        "name" .= name, "score" .= score, "endBonus" .= endBonus, "connected" .= connected, "lastSeen" .= lastSeen
        ]

data ClientPlayer = ClientPlayer {name:: Text, score:: Int, endBonus:: Maybe Int, connected :: Bool, lastSeen :: Maybe Int}

-- TODO: get scores from game player model
fromServerPlayer :: ServerPlayer -> ClientPlayer
fromServerPlayer (ServerPlayer name _ _ numConnections lastActive) =
    ClientPlayer (playerName name) 0 Nothing (numConnections > 0) (map toMillisecondsSinceUnixEpoch lastActive)
    where
        playerName Nothing = ""
        playerName (Just n) = n

        toMillisecondsSinceUnixEpoch :: UTCTime -> Int
        toMillisecondsSinceUnixEpoch utcTime = 1000 * (round (utcTimeToPOSIXSeconds utcTime))
