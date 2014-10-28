module Series (digits, slices, largestProduct) where
import Data.List (tails)
import Safe(maximumDef)

digits :: (Read a, Num a) => String -> [a]
digits = map (read . (:[]))

consecutives ::  Int -> [a] -> [[a]]
consecutives n = filter ((==n) . length) . map (take n) . tails

slices :: (Read a, Num a) => Int -> String -> [[a]]
slices n = consecutives n . digits

largestProduct :: (Read a, Num a,Ord a) => Int -> String -> a
largestProduct n = maximumDef 1 . map product . slices n
