module Handler.Home.Model.Outbound where

import ClassyPrelude
import Data.Aeson (ToJSON (..), (.=), object)
import Model.GameSetup (TileValues)

data OtherPlayer = OtherPlayer { playerName :: Text, playerActive :: Bool }

instance ToJSON OtherPlayer where
  toJSON (OtherPlayer name active) = object
    [ "name"   .= name
    , "active" .= active
    ]

data ActiveGameSummary = ActiveGameSummary
  { gameId       :: Text
  , boardString  :: Text
  , yourMove     :: Bool
  , lastActivity :: Maybe UTCTime
  , tileValues   :: TileValues
  , otherPlayers :: [OtherPlayer]
  }

instance ToJSON ActiveGameSummary where
  toJSON (ActiveGameSummary gId bString yMove lastAct tVals oPlayers) = object
    [ "gameId"       .= gId
    , "boardString"  .= bString
    , "yourMove"     .= yMove
    , "lastActivity" .= lastAct
    , "tileValues"   .= tVals
    , "otherPlayers" .= oPlayers
    ]

data OutboundHomeMessage
  = GamesUpdate [ActiveGameSummary]

instance ToJSON OutboundHomeMessage where
  toJSON (GamesUpdate summaries) = object
    [ "command" .= ("gamesUpdate" :: Text)
    , "payload" .= summaries
    ]
