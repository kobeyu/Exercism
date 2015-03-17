module Connect(resultFor, Color(Black, White)) where
import Data.Tuple(swap)
import qualified Data.Set as S

data Color = Black | White deriving (Eq,Show)

type CellLoc = (Int,Int)
type Board = (Int,S.Set CellLoc)

mkBoard :: Char -> (CellLoc -> CellLoc) -> [String] -> Board
mkBoard _ _ [] = (0,S.empty)
mkBoard c f s = (width,set)
    where
        width = fst . f $ (length . head $ s,length s)
        set = S.fromList [f (x,y) | (y,r) <- zip [1..] s, (x,e) <- zip [1..] r, e == c]

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
whiteWin = lrConnect . mkBoard 'O' swap

blackWin :: [String] -> Bool
blackWin = lrConnect . mkBoard 'X' id

resultFor :: [String] -> Maybe Color
resultFor b
    |blackWin b = Just Black
    |whiteWin b = Just White
    |otherwise = Nothing
