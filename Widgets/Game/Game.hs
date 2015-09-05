module Widgets.Game.Game (emptyGame) where

    import Import
    import Wordify.Rules.Game
    import Wordify.Rules.Board
    import Widgets.Game.Board
    import Widgets.Game.ChatBox
    import Widgets.Game.Rack
    import Widgets.Game.ScoreBoard
    import Controllers.Game.Model.ServerGame

    emptyGame :: Widget
    emptyGame = $(widgetFile "game")

        where
            currentBoard = emptyBoard
            currentPlayers = []
            movesPlayed = []
            tiles = []

    gameInPlay :: ServerGame -> Widget
    gameInPlay serverGame = $(widgetFile "game")
        where
            gameInProgress = game serverGame
            currentBoard = board gameInProgress
            currentPlayers = players gameInProgress
            movesPlayed = reverse $ moveSummaries serverGame
            tiles = []