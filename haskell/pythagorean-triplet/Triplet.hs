module Triplet (mkTriplet, isPythagorean, pythagoreanTriplets) where
import           Data.List                        (sort)
import           Math.NumberTheory.Powers.Squares (isSquare')

data Trip a = Trip a a | NoTrip deriving (Show,Eq)

mkTriplet :: (Ord a, Num a) => a -> a -> a -> Trip a
mkTriplet x y z
    | a^2 + b^2 == c^2 = Trip a b
    | otherwise = NoTrip
    where [a,b,c] = sort [x,y,z]

isPythagorean :: Trip a -> Bool
isPythagorean NoTrip = False
isPythagorean _ = True

pythagoreanTriplets :: Integral a => a -> a -> [Trip a]
pythagoreanTriplets min max = [Trip a b | a <- [min..max], b <- [a..max], let c2 = a^2+b^2, c2 <= max^2, isSquare' c2 ]

