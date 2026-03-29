{-# LANGUAGE OverloadedStrings #-}

module Handler.BoardSvg where

import Import
import Modules.Games.Api (getGame)
import Controllers.Game.Model.ServerGame (ServerGame (game))
import Wordify.Rules.Board (Board, allSquares)
import Wordify.Rules.Square (Square (..), tileIfOccupied)
import Wordify.Rules.Tile (Tile (..), tileString)
import Wordify.Rules.Pos (Pos, xPos, yPos)
import Wordify.Rules.Game (Game (board))
import qualified Data.Text as T

getGameBoardSvgR :: Text -> Handler TypedContent
getGameBoardSvgR gameId = do
  app <- getYesod
  result <- runResourceT $ do
    (_, gameResult) <- getGame (gameService app) gameId
    case gameResult of
      Left err -> pure (Left err)
      Right serverGame -> do
        gameSoFar <- liftIO (readTVarIO (game serverGame))
        pure (Right (board gameSoFar))
  case result of
    Left err -> invalidArgs [err]
    Right brd -> do
      cacheSeconds 30
      pure $ TypedContent "image/svg+xml" $ toContent (boardToSvg brd)

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
      tileSvg = case tileIfOccupied square >>= tileString of
        Just letterStr ->
          let letter = T.pack letterStr
              cx = T.pack (show (x + sz `div` 2))
              cy = T.pack (show (y + sz `div` 2 + 4))
          in T.concat
               [ "<text x=\"" , cx , "\" y=\"" , cy
               , "\" text-anchor=\"middle\" font-family=\"Arial,sans-serif\" font-size=\"10\" font-weight=\"bold\" fill=\"#78350f\">"
               , letter , "</text>"
               ]
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
