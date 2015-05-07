module Handler.Game(getGameR) where

import Import
import Widgets.Board.Board

getGameR :: Handler Html
getGameR = 
    defaultLayout $
        do
            setTitle "Test"
            gameWidget
    where
        gameWidget = $(widgetFile "game")
        boardWidget = initialBoard
        tiles = repeat 4 $ Letter 'A' 1

