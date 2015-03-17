module Connect(resultFor, Color(Black, White)) where
import Data.List(transpose)
import qualified Data.Set as S

data Color = Black | White deriving (Eq,Show)

type CellLoc = (Int,Int)
type Board = (Int,S.Set CellLoc)

mkBoard :: Char -> [String] -> Board
mkBoard c s = (width, S.fromList [(x,y) | (y,r) <- zip [1..] s, (x,e) <- zip[1..] r, e == c])
    where width = length . head $ s

neighboursWhere :: (CellLoc -> Bool) -> CellLoc -> S.Set CellLoc
neighboursWhere predicate (x,y)
    = S.fromList . filter predicate $ [(x-1,y), (x-1,y+1), (x,y-1), (x,y+1), (x+1,y-1), (x+1,y)]

lrConnect :: Board -> Bool
lrConnect (wid,b) = f leftCol
    where
        leftCol = S.filter ((==1) . fst) b
        f :: S.Set (Int,Int) -> Bool
        f connected = good || (newConnected /= connected && f newConnected)
            where
            good = not . S.null . S.filter ((==wid) . fst) $ newConnected
            expand = S.union . neighboursWhere (`S.member` b)
            newConnected = S.foldr' expand connected connected

whiteWin :: [String] -> Bool
whiteWin = lrConnect . mkBoard 'O' . transpose

blackWin :: [String] -> Bool
blackWin = lrConnect . mkBoard 'X'

resultFor :: [String] -> Maybe Color
resultFor b
    |blackWin b = Just Black
    |whiteWin b = Just White
    |otherwise = Nothing
