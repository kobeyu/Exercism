module Matrix (saddlePoints) where
import Data.Array.IArray
import Control.Arrow((&&&))
import Data.List(intersect)
import Data.Map(Map,toAscList,toDescList,fromListWith)

toMap :: (Ord b) => (a -> b) -> [a] -> Map b [a]
toMap f = fromListWith (++) . map (f&&&(:[]))

minimumsBy :: (Ord b) => (a -> b) -> [a] -> [a]
minimumsBy f = snd . head . toAscList . toMap f

maximumsBy :: (Ord b) => (a -> b) -> [a] -> [a]
maximumsBy f = snd . head . toDescList . toMap f

saddlePoints :: (IArray a e, Ord e,Show e) => a (Int,Int) e -> [(Int,Int)]
saddlePoints a = maxRows `intersect` minCols
    where
        ((minX,minY),(maxX,maxY)) = bounds a
        rows = map (flip zip [minY..maxY] . repeat) [minX..maxX]
        cols = map (zip [minX..maxX] . repeat ) [minY..maxY]
        maxRows = concatMap (maximumsBy (a!)) rows
        minCols = concatMap (minimumsBy (a!)) cols
