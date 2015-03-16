module Connect(resultFor, Color(Black, White)) where
import Data.List(transpose)
import qualified Data.Array as A(array,bounds,(!),assocs, Array() )
import qualified Data.Set as S(toList,fromList,Set(),map)

data Color = Black | White deriving (Eq,Show)

type CellLoc = (Int,Int)
type Board = A.Array CellLoc Bool

mkBoard :: Char -> [String] -> Board
mkBoard c s = A.array ((1,1),(width,height)) assocs
    where
        height = length s
        width = length $ head s
        assocs = [((x,y),e == c) | (y,r) <- zip [1..] s, (x,e) <- zip [1..] r]

neighbours :: CellLoc -> (CellLoc,CellLoc) -> [CellLoc]
neighbours (x,y) ((minX,minY),(maxX,maxY))
    = [(x',y') | x' <- [x-1,x,x+1],y' <- [y-1,y,y+1]
                 , x' >= minX, x' <= maxX, y' >= minY, y' <= maxY
                 , (x',y') `notElem` [(x-1,y-1),(x+1,y+1)]]

lrConnect :: Board -> Bool
lrConnect b = f leftCol
    where
        leftCol = S.fromList . map fst . filter (\((a,_),e) -> a == 1 && e) . A.assocs $ b
        ((_,_),(maxX,_)) = A.bounds b
        f :: S.Set (Int,Int) -> Bool
        f connected = good || (newConnected /= connected && f newConnected)
            where
            good = any ((==maxX) . fst) $ S.toList newConnected
            newConnected = S.fromList [p | c <- S.toList connected, p <- neighbours c (A.bounds b), b A.! p]

whiteWin :: [String] -> Bool
whiteWin = lrConnect . mkBoard 'O' . transpose

blackWin :: [String] -> Bool
blackWin = lrConnect . mkBoard 'X'

resultFor :: [String] -> Maybe Color
resultFor b
    |blackWin b = Just Black
    |whiteWin b = Just White
    |otherwise = Nothing
