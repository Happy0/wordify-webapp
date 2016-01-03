module Controllers.Game.Model.ServerPlayer (ServerPlayer(ServerPlayer), 
                                            name, 
                                            identifier,
                                            makeNewPlayer,
                                            makeGameStatePlayers,
                                            makeNewPlayerId) where

    import Data.Text
    import Prelude
    import Network.Mail.Mime
    import Control.Applicative
    import System.Random
    import qualified Wordify.Rules.Player as G

    data ServerPlayer = ServerPlayer {name :: Text, identifier :: Text}

    makeNewPlayer :: Text -> Text -> ServerPlayer
    makeNewPlayer playerName gameId = ServerPlayer playerName gameId

    makeNewPlayerId :: StdGen -> (Text, StdGen)
    makeNewPlayerId generator =
            let (result, newGen) = randomString 8 generator
            in (pack result, newGen)

    makeGameStatePlayers :: Int -> Either Text (G.Player, G.Player, Maybe (G.Player, Maybe G.Player))
    makeGameStatePlayers numPlayers
        | numPlayers == 2 = Right (G.makePlayer "player1", G.makePlayer "player2", Nothing)
        | numPlayers == 3 = Right (G.makePlayer "player1", G.makePlayer "player2", Just ((G.makePlayer "player3"), Nothing))
        | numPlayers == 4 = Right (G.makePlayer "player1", G.makePlayer "player2", Just ((G.makePlayer "player3"), Just (G.makePlayer "player4")))
        | otherwise = Left "Invalid number of players"
