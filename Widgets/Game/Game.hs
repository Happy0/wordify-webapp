module Widgets.Game.Game (emptyGame) where

    import Import
    import Wordify.Rules.Game
    import Wordify.Rules.Board
    import Widgets.Game.Board
    import Widgets.Game.ChatBox
    import Widgets.Game.Rack
    import Widgets.Game.ScoreBoard

    emptyGame :: Widget
    emptyGame = $(widgetFile "game")

        where
            currentBoard = emptyBoard
            currentPlayers = []
            movesPlayed = []
            tiles = []

    gameInPlay :: Game -> Widget
    gameInPlay game = $(widgetFile "game")
        where
            currentBoard = board game
            currentPlayers = players game
            movesPlayed = []
            tiles = []