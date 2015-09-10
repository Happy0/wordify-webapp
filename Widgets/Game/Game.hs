module Widgets.Game.Game (emptyGame, gameInPlay) where

    import Import
    import Wordify.Rules.Game
    import Wordify.Rules.Board
    import Wordify.Rules.Player
    import Widgets.Game.Board
    import Widgets.Game.ChatBox
    import Widgets.Game.Rack
    import Widgets.Game.ScoreBoard
    import Controllers.Game.Model.ServerGame
    import qualified Data.List as L

    emptyGame :: Widget
    emptyGame = $(widgetFile "game")

        where
            currentBoard = emptyBoard
            currentPlayers = []
            movesPlayed = []
            tiles = []

    gameInPlay :: ServerGame -> Maybe Int -> Widget
    gameInPlay serverGame playerNumber = $(widgetFile "game")
        where
            gameInProgress = game serverGame
            currentBoard = board gameInProgress
            currentPlayers = players gameInProgress
            movesPlayed = L.reverse $ moveSummaries serverGame
            tiles = maybe [] id $ tilesOnRack <$> (playerNumber >>= getPlayer currentPlayers)

    getPlayer :: [Player] -> Int -> Maybe Player
    getPlayer players playerNumber
        | (playerNumber >= 1) && (playerNumber <= L.length players) = Just $ players L.!! (playerNumber - 1)
        | otherwise = Nothing