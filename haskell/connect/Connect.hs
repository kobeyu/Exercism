module Connect(resultFor, Color(Black, White)) where
import Data.List(transpose)
import qualified Data.Array as A
import qualified Data.Set as S
import Control.Monad(liftM2)

data Color = Black | White deriving (Eq,Show)

type CellLoc = (Int,Int)
type Board = A.Array CellLoc Bool

mkBoard :: Char -> [String] -> Board
mkBoard c s = A.array ((1,1),(width,height)) assocs
    where
        height = length s
        width = length $ head s
        assocs = [((x,y),e == c) | (y,r) <- zip [1..] s, (x,e) <- zip [1..] r]

neighboursWhere :: (CellLoc -> Bool) -> CellLoc -> S.Set CellLoc
neighboursWhere predicate (x,y)
    = S.fromList . filter predicate $ [(x-1,y), (x-1,y+1), (x,y-1), (x,y+1), (x+1,y-1), (x+1,y)]

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftM2 (&&)

lrConnect :: Board -> Bool
lrConnect b = f leftCol
    where
        leftCol = S.fromList [(x,y) | ((x,y),e) <- A.assocs b, x == 1 && e]
        bnds@((_,_),(maxX,_)) = A.bounds b
        f :: S.Set (Int,Int) -> Bool
        f connected = good || (newConnected /= connected && f newConnected)
            where
            good = not . S.null . S.filter ((==maxX) . fst) $ newConnected
            expand = S.union . neighboursWhere (A.inRange bnds &&& (b A.!))
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
