module Matrix ( Matrix, row, column, rows, cols, shape, transpose
              , reshape, flatten, fromString, fromList) where
import qualified Data.Vector as V
import Data.List(unfoldr)
import Data.Maybe(listToMaybe)
import Control.Arrow((&&&))

data Matrix a = Mat {cols :: Int, flatten :: V.Vector a}
	deriving (Show,Eq)

fromString :: (Read a) => String -> Matrix a
fromString = fromList . map (unfoldr $ listToMaybe . reads) . lines

fromList :: [[a]] -> Matrix a
fromList = uncurry Mat . (width &&& V.fromList . concat)
	where 
		width :: [[a]] -> Int
		width = maybe 0 length . listToMaybe

row :: Int -> Matrix a -> V.Vector a
row n (Mat w v) = V.slice (n*w) w v

column :: Int -> Matrix a -> V.Vector a
column n m@(Mat w v) = V.backpermute v . V.generate (rows m) $ (n +) . (w *)

rows :: Matrix a -> Int
rows (Mat 0 _) = 0
rows (Mat a v) = V.length v `div` a

shape :: Matrix a -> (Int, Int)
shape = rows&&&cols

transpose :: Matrix a -> Matrix a
transpose m@(Mat w v) = Mat rowsm (V.backpermute v vint)
	where
		rowsm = rows m
		len = V.length v
		vint :: V.Vector Int
		vint = V.generate len transformIndex
		transformIndex :: Int -> Int
		transformIndex i = let (r,c) = i `quotRem` rowsm in c*w+r

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (w, _) (Mat _ v)= Mat w v
