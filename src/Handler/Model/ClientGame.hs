module Handler.Model.ClientGame where

import ClassyPrelude (Text, Maybe (..), Int, Bool, (>), UTCTime, RealFrac (..), map, (*), not, undefined, fmap, (-), sum, zip)
import Data.Aeson
import Controllers.Game.Model.ServerPlayer
import Data.Time.Clock.POSIX
import qualified Wordify.Rules.Player as P
import Wordify.Rules.Tile
import qualified Data.Text as T
import Controllers.Game.Api (MoveSummary (BoardMoveSummary, PassMoveSummary, ExchangeMoveSummary, GameEndSummary))

instance ToJSON ClientPlayer where
    toJSON (ClientPlayer name score endBonus connected lastSeen) = object [
        "name" .= name, "score" .= score, "endBonus" .= endBonus, "connected" .= connected, "lastSeen" .= lastSeen
        ]

instance ToJSON ClientTile where
    toJSON (ClientLetter letter value) = object ["type" .= T.pack "letter", "letter" .= letter, "value" .= value]
    toJSON (ClientBlank assigned) = object [ "type" .= T.pack "blank", "assigned" .= assigned]

instance ToJSON ClientMoveHistoryWordMade where
    toJSON (ClientMoveHistoryWordMade word score) = object ["word" .= word, "score" .= score]

instance ToJSON ClientMoveHistoryEntry where
    toJSON (ClientPass playerIndex) = object [ "type" .= T.pack "pass", "playerIndex" .= playerIndex ]
    toJSON (ClientExchange playerIndex) = object [ "type" .= T.pack "exchange", "playerIndex" .= playerIndex]
    toJSON (ClientMove playerIndex overallScore moves) = object [ 
        "type" .= T.pack "boardMove",
        "playerIndex" .= playerIndex,
        "overallScore" .= overallScore,
        "wordsMade" .= moves
        ]

data ClientPlayer = ClientPlayer {name:: Text, score:: Int, endBonus:: Maybe Int, connected :: Bool, lastSeen :: Maybe Int}
data ClientTile = ClientLetter {letter :: Text, value:: Int} | ClientBlank { assigned :: Maybe Text}

data ClientMoveHistoryWordMade = ClientMoveHistoryWordMade { clientMoveHistoryWordMade :: Text, clientMoveHistoryScore :: Int} 

data ClientMoveHistoryEntry = 
    ClientPass {clientPassPlayerIndex :: Int }
    | ClientExchange { clientExchangePlayerIndex :: Int}
    | ClientMove { clientMovePlayerIndex :: Int, clientMoveOverallScore :: Int, clientMoveWordsMade :: [ClientMoveHistoryWordMade] } 

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

fromServerMoveHistory :: [MoveSummary] -> [ClientMoveHistoryEntry]
fromServerMoveHistory moveSummaries = map toClientEntry (zip [0..] moveSummaries)
    where
        toClientEntry (playerIndex, BoardMoveSummary score wordsAndScores _ _) =
            ClientMove playerIndex score (map (\(word, wordScore) -> ClientMoveHistoryWordMade word wordScore) wordsAndScores)
        toClientEntry (playerIndex, PassMoveSummary) = ClientPass playerIndex
        toClientEntry (playerIndex, ExchangeMoveSummary) = ClientExchange playerIndex
        toClientEntry (playerIndex, GameEndSummary Nothing _) = ClientPass playerIndex
        toClientEntry (playerIndex, GameEndSummary (Just wordsAndScores) _) =
            ClientMove playerIndex (sum (map (\(_, wordScore) -> wordScore) wordsAndScores)) (map (\(word, wordScore) -> ClientMoveHistoryWordMade word wordScore) wordsAndScores)