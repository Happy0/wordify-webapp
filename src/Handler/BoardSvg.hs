{-# LANGUAGE OverloadedStrings #-}

module Handler.BoardSvg where

import Import
import Control.Error (ExceptT (..), runExceptT)
import Modules.Games.Api (getGame, GameService)
import Controllers.Game.Model.ServerGame (ServerGame (game))
import Wordify.Rules.Board (Board, allSquares, emptyBoard)
import Wordify.Rules.Square (Square (..), tileIfOccupied)
import Wordify.Rules.Tile (Tile (..), tileString)
import Wordify.Rules.Pos (Pos, xPos, yPos)
import Wordify.Rules.Game (Game (board))
import qualified Data.Text as T
import Data.Either (fromRight)

getGameBoardSvgR :: Text -> Handler TypedContent
getGameBoardSvgR gameId = do
  app <- getYesod
  boardResult <- liftIO $ getGameBoard (gameService app) gameId
  let board = fromRight emptyBoard boardResult
  cacheSeconds 30
  pure $ TypedContent "image/svg+xml" $ toContent (boardToSvg board)
  where
    getGameBoard :: GameService -> Text -> IO (Either Text Board)
    getGameBoard gameService gameId = do
      runResourceT $ runExceptT $ do
        serverGame <- ExceptT $ snd <$> getGame gameService gameId
        liftIO $ board <$> readTVarIO (game serverGame)

-- | Size of each square in the SVG grid
squareSize :: Int
squareSize = 13

-- | Total SVG dimension (15 * squareSize + 2 for border)
svgDimension :: Int
svgDimension = 15 * squareSize

boardToSvg :: Board -> Text
boardToSvg brd =
  let squares = allSquares brd
      dim = T.pack (show svgDimension)
      squareSvgs = T.concat (map renderSquare squares)
  in T.concat
       [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
       , "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" , dim , "\" height=\"" , dim
       , "\" viewBox=\"0 0 " , dim , " " , dim , "\">"
       , "<rect width=\"" , dim , "\" height=\"" , dim , "\" fill=\"#92400e\"/>"
       , squareSvgs
       , "</svg>"
       ]

renderSquare :: (Pos, Square) -> Text
renderSquare (pos, square) =
  let col = xPos pos - 1
      row = yPos pos - 1
      x = col * squareSize
      y = row * squareSize
      sz = squareSize
      xT = T.pack (show x)
      yT = T.pack (show y)
      szT = T.pack (show sz)
      hasTile = isJust (tileIfOccupied square)
      isBlank = case tileIfOccupied square of
        Just (Blank _) -> True
        _              -> False
      bgColor = if hasTile then tileColor isBlank else squareColor square
      tileSvg = case tileIfOccupied square of
        Just tile ->
          let letterPart = case tileString tile of
                Just letterStr ->
                  let letter = T.pack letterStr
                      cx = T.pack (show (x + sz `div` 2))
                      cy = T.pack (show (y + sz `div` 2 + 3))
                  in T.concat
                       [ "<text x=\"" , cx , "\" y=\"" , cy
                       , "\" text-anchor=\"middle\" font-family=\"Arial,sans-serif\" font-size=\"9\" font-weight=\"bold\" fill=\"#78350f\">"
                       , letter , "</text>"
                       ]
                Nothing -> ""
              valuePart = case tile of
                Letter _ v ->
                  let vx = T.pack (show (x + sz - 2))
                      vy = T.pack (show (y + sz - 1))
                  in T.concat
                       [ "<text x=\"" , vx , "\" y=\"" , vy
                       , "\" text-anchor=\"end\" font-family=\"Arial,sans-serif\" font-size=\"4\" fill=\"#92400e\">"
                       , T.pack (show v) , "</text>"
                       ]
                Blank _ -> ""
          in letterPart <> valuePart
        Nothing -> ""
  in T.concat
       [ "<rect x=\"" , xT , "\" y=\"" , yT , "\" width=\"" , szT , "\" height=\"" , szT
       , "\" fill=\"" , bgColor , "\" stroke=\"#92400e\" stroke-width=\"0.5\"/>"
       , tileSvg
       ]

tileColor :: Bool -> Text
tileColor True  = "#fffbeb" -- bg-amber-50 (blank tile)
tileColor False = "#fef3c7" -- bg-amber-100 (regular tile)

squareColor :: Square -> Text
squareColor (TripleWord _)   = "#ef4444" -- bg-red-500
squareColor (DoubleWord _)   = "#f9a8d4" -- bg-pink-300
squareColor (TripleLetter _) = "#3b82f6" -- bg-blue-500
squareColor (DoubleLetter _) = "#bae6fd" -- bg-sky-200
squareColor (Normal _)       = "#fffbeb" -- bg-amber-50
