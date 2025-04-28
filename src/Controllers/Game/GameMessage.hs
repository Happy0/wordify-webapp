module Controllers.Game.GameMessage
  ( MoveSummary (BoardMoveSummary, PassMoveSummary, ExchangeMoveSummary, GameEndSummary),
    GameMessage (PlayerBoardMove, GameEnd, PlayerPassMove, PlayerExchangeMove, PlayerChat, PlayerConnect, PlayerDisconnect, WordDefinitions),
    ChatMessage (ChatMessage),
  )
where

import Controllers.Definition.DefinitionService (Definition)
import Data.Maybe (Maybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Wordify.Rules.Player (Player)
import Wordify.Rules.Pos
import Wordify.Rules.Tile (Tile)
import Prelude (Int)

data ChatMessage = ChatMessage {user :: Text, message :: Text, when :: UTCTime, messageNumber :: Int}

data MoveSummary
  = BoardMoveSummary
      { overallScore :: Int,
        wordsAndScores :: [(Text, Int)],
        moveDirection :: Direction,
        placedTiles :: [Pos]
      }
  | PassMoveSummary
  | ExchangeMoveSummary
  | GameEndSummary (Maybe [(Text, Int)]) [Player]

-- Messages sent over the server game channel to notify clients of changes
-- such as a player making a move successfully
data GameMessage
  = PlayerBoardMove
      { moveNumber :: Int,
        placed :: [(Pos, Tile)],
        summary :: MoveSummary,
        players :: [Player],
        nowPlaying :: Int,
        tilesRemaining :: Int
      }
  | GameEnd Int (Maybe [(Pos, Tile)]) MoveSummary
  | PlayerPassMove Int Int MoveSummary
  | PlayerExchangeMove Int Int [Tile] MoveSummary
  | PlayerChat ChatMessage
  | PlayerConnect Int UTCTime
  | PlayerDisconnect Int UTCTime
  | WordDefinitions Text UTCTime [Definition] Int