module Matrix (saddlePoints) where
import Data.Array.IArray
import Data.Function(on)
import Control.Arrow((&&&))
import Data.List(minimumBy,maximumBy,intersect,group,sort,groupBy)
import Debug.Trace

minimums :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
minimums f = head . groupBy f . sort

maximums :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
maximums f = head . groupBy f . reverse . sort

saddlePoints :: (IArray a e, Ord e) => a (Int,Int) e -> [(Int,Int)]
saddlePoints a = intersect maxRows minCols 
    where
        maxRows :: [(Int,Int)]
        maxRows = _snd $ maximums ((==) `on` fst) (c rows)
        minCols :: [(Int,Int)]
        minCols = _snd $ minimums ((==) `on` fst) (c cols)
        cols = map (zip [minX..maxX] . repeat) [minY..maxY]
        c = map ((a!) &&& id)
        rows = map (flip zip [minY..maxY] . repeat) [minX..maxX]
        ((minX,minY),(maxX,maxY)) = bounds a
