module Handler.Home.Model.Outbound where

import ClassyPrelude
import Data.Aeson (ToJSON (..), (.=), object)
import Data.Time.Clock (UTCTime)
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

data TvActiveGameSummary = TvActiveGameSummary
  { tvGameId      :: Text
  , tvBoardString :: Text
  , tvLastActivity :: UTCTime
  , tvTileValues  :: TileValues
  , tvPlayers     :: [OtherPlayer]
  }

instance ToJSON TvActiveGameSummary where
  toJSON (TvActiveGameSummary gId bStr lastAct tVals players) = object
    [ "gameId"       .= gId
    , "boardString"  .= bStr
    , "lastActivity" .= lastAct
    , "tileValues"   .= tVals
    , "players"      .= players
    ]

data OutboundHomeMessage
  = GamesUpdate [ActiveGameSummary]
  | TvUpdate TvActiveGameSummary

instance ToJSON OutboundHomeMessage where
  toJSON (GamesUpdate summaries) = object
    [ "command" .= ("gamesUpdate" :: Text)
    , "payload" .= summaries
    ]
  toJSON (TvUpdate summary) = object
    [ "command" .= ("tvUpdate" :: Text)
    , "payload" .= summary
    ]
