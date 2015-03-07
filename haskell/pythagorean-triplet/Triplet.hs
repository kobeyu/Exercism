module Triplet (mkTriplet, isPythagorean, pythagoreanTriplets) where
import           Data.List                        (sort)
import           Math.NumberTheory.Powers.Squares (isSquare')

data Trip a = Trip a a | NoTrip deriving (Show,Eq)

mkTriplet :: (Ord a, Num a) => a -> a -> a -> Trip a
mkTriplet x y z
    | a*a + b*b == c*c = Trip a b
    | otherwise = NoTrip
    where [a,b,c] = sort [x,y,z]

isPythagorean :: Trip a -> Bool
isPythagorean NoTrip = False
isPythagorean _ = True

pythagoreanTriplets :: Integral a => a -> a -> [Trip a]
pythagoreanTriplets lo hi = [Trip a b | a <- [lo..hi], b <- [a..hi], let c2 = a*a+b*b, c2 <= hi*hi, isSquare' c2 ]

