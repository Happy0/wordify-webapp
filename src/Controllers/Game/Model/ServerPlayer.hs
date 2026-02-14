module Controllers.Game.Model.ServerPlayer
  ( ServerPlayer (ServerPlayer),
    user,
    playerId,
    playerUsername,
    makeNewPlayer,
    addConnection,
    removeConnection,
    makeGameStatePlayers,
    makeNewPlayerId,
    numConnections,
    lastActive,
  )
where

import ClassyPrelude (UTCTime)
import Controllers.User.Model.ServerUser (ServerUser)
import qualified Controllers.User.Model.ServerUser as SU
import Data.Text as T
import Network.Mail.Mime
import System.Random
import qualified Wordify.Rules.Player as G
import Prelude

data ServerPlayer = ServerPlayer {
  user :: ServerUser,
  gameId :: Text,
  numConnections :: Int,
  lastActive :: Maybe UTCTime
}

playerId :: ServerPlayer -> Text
playerId = SU.userId . user

playerUsername :: ServerPlayer -> Maybe Text
playerUsername = SU.username . user

makeNewPlayer :: ServerUser -> Text -> Int -> Maybe UTCTime -> ServerPlayer
makeNewPlayer serverUser gameId = ServerPlayer serverUser gameId

addConnection :: ServerPlayer -> UTCTime -> ServerPlayer
addConnection player time = player {numConnections = numConnections player + 1, lastActive = Just time}

removeConnection :: ServerPlayer -> UTCTime -> ServerPlayer
removeConnection player time = player {numConnections = numConnections player - 1, lastActive = Just time}

makeNewPlayerId :: StdGen -> (Text, StdGen)
makeNewPlayerId generator =
  let (result, newGen) = randomString 8 generator
   in (pack result, newGen)

makeGameStatePlayers :: Int -> Either Text (G.Player, G.Player, Maybe (G.Player, Maybe G.Player))
makeGameStatePlayers numPlayers
  | numPlayers == 2 = Right (G.makePlayer "player1", G.makePlayer "player2", Nothing)
  | numPlayers == 3 = Right (G.makePlayer "player1", G.makePlayer "player2", Just (G.makePlayer "player3", Nothing))
  | numPlayers == 4 = Right (G.makePlayer "player1", G.makePlayer "player2", Just (G.makePlayer "player3", Just (G.makePlayer "player4")))
  | otherwise = Left $ T.concat ["Invalid number of players: ", pack (show numPlayers)]
