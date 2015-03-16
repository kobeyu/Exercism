module Connect(resultFor, Color(Black, White)) where
import Data.List(transpose)
import qualified Data.Array as A

data Color = Black | White deriving (Eq,Show)

data Cell = Empty | Full {left,right :: Bool}

type Board = A.Array (Int, Int) Cell

mkBoard :: [String] -> Char -> Board
mkBoard s = A.array ((1,1),(width,height)) assocs
    where
        height = length s
        width = length $ head s
        assocs = undefined


isWin :: [[Bool]] -> Bool
isWin = undefined

whiteWin :: [String] -> Bool
whiteWin = isWin . transpose . (map.map) (=='O')

blackWin :: [String] -> Bool
blackWin = isWin . (map . map) (=='X')

resultFor :: [String] -> Maybe Color
resultFor b
    |blackWin b = Just Black
    |whiteWin b = Just White
    |otherwise = Nothing
