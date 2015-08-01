module Widgets.Game.TestGame (testGame, moves) where

    import Wordify.Rules.Dictionary
    import qualified Data.Map as M
    import Wordify.Rules.ScrabbleError
    import Wordify.Rules.LetterBag
    import Wordify.Rules.Pos
    import Wordify.Rules.Tile
    import Wordify.Rules.Board
    import Data.Maybe
    import Wordify.Rules.Move
    import Wordify.Rules.Player
    import Wordify.Rules.Game
    import Wordify.Rules.Move
    import qualified Data.List.NonEmpty as NE
    import Data.Char
    import qualified Data.Sequence as Seq
    import qualified System.FilePath as F
    import Control.Monad   
    import Data.List
    import Control.Applicative
    import Prelude

    data Direction = Horizontal | Vertical

    letterValues :: M.Map Char Int
    letterValues = M.fromList $ [('A', 1), ('B',3), ('C', 3), ('D', 2), ('E', 1), ('F',4),('G',2),('H',4),('I',1),('J',8),('K',5),('L',1) ,('M',3),('N',1),('O',1),('P',3),('Q',10),('R', 1), ('S',1), ('T',1),('U',1),('V',4),('W',4),('X',8),('Y',4),('Z',10)]

    testDictionary :: IO (Either ScrabbleError Dictionary)
    testDictionary = makeDictionary $ "config" ++ [F.pathSeparator] ++ "dictionaries" ++ [F.pathSeparator] ++ "en.txt"

    letterBag :: IO LetterBag
    letterBag = bagFromTiles $ map toTileBag tilesAsLetters
        where
            tilesAsLetters = "JEARVINENVO_NILLEWBKONUIEUWEAZBDESIAPAEOOURGOCDSNIADOAACAR_RMYELTUTYTEREOSITNIRFGPHAQLHESOIITXFDMETG"

    testGame :: IO (Either ScrabbleError Game)
    testGame =
      do
        bag <- letterBag
        dict <- testDictionary
        return $ resultGame bag dict
      where
        resultGame bag dict =
          do
            dc <- dict
            let [player1, player2,player3,player4] = map makePlayer ["player 1","player 2","player 3","player 4"]
            makeGame (player1, player2, Just (player3, Just player4)) bag dc

    placeMap :: String -> Direction -> (Int, Int) -> M.Map Pos Tile 
    placeMap letters direction pos = M.fromList $ zip positions tiles
        where
            positions =
                case direction of
                    Horizontal -> catMaybes $ takeWhile isJust <$> map posAt $ iterate (\(x,y) -> (x+1,y)) pos
                    Vertical -> catMaybes $ takeWhile isJust <$> map posAt $ iterate (\(x,y) -> (x, y + 1)) pos

            tiles = map toTilePlaced letters

    toTileBag :: Char -> Tile
    toTileBag lettr = 
        case lettr of
            '_' -> Blank Nothing
            x -> Letter x $ M.findWithDefault 0 x letterValues

    toTilePlaced :: Char -> Tile
    toTilePlaced char
        | isLower char = Blank $ Just (toUpper char)
        | otherwise = toTileBag char

    moves :: [Move]
    moves = moveList
        where
            moveList = 
                map PlaceTiles [
                      placeMap "RAVINE" Horizontal (8,8)
                    , placeMap "OVEl" Vertical (12,9)
                    , placeMap "W" Vertical (9,7) `M.union` placeMap "KE" Vertical (9,9)
                    , placeMap "N" Horizontal (11,9)
                    , placeMap "B" Horizontal (13,7) `M.union` placeMap "D" Horizontal (13,9)
                    , placeMap "NAI" Horizontal (9,12)
                    , placeMap "B" Horizontal (11,11) `M.union` placeMap "LLE" Horizontal (13,11)
                    , placeMap "WEE" Vertical (10,13)
                    , placeMap "JA" Vertical (15,9) `M.union` placeMap "GERS" Vertical (15,12)
                    , placeMap "CANOPI" Horizontal (4,15) `M.union` placeMap "D" Horizontal (11,15)
                    , placeMap "SONI" Vertical (4,11)
                    , placeMap "AUDIO" Vertical (3,10)
                    , placeMap "RAZeR" Vertical (5,8)
                    , placeMap "MULEY" Vertical (2,6)
                    , placeMap "ROOTY" Vertical (3,2)
                    , placeMap "ETUIS" Vertical (14,4)
                    , placeMap "RACING" Vertical (1,10)
                    , placeMap "HATP" Vertical (11,4)
                    , placeMap "HAES" Vertical (12,2)
                    , placeMap "DOUX" Vertical (15,1)
                    , placeMap "GEM" Vertical (13,1)
                    , placeMap "Q" Horizontal (4,9) `M.union` placeMap "T" Horizontal (6,9)
                    , placeMap "IO" Vertical (6,13)
                    , placeMap "FIT" Vertical (10,2)]