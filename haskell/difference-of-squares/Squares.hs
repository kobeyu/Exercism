module Squares (sumOfSquares, squareOfSums, difference) where
import           Control.Monad (liftM2)

sumOfSquares :: (Integral a) => a -> a
sumOfSquares n = (n*(n+1)*(2*n+1)) `div` 6

squareOfSums :: (Integral a) => a -> a
squareOfSums n = ((n*(n+1)) `div` 2)^(2::Int)

difference :: (Integral a) => a -> a
difference = liftM2 subtract sumOfSquares squareOfSums
