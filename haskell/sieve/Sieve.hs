module Sieve (primesUpTo) where

primesUpTo :: (Integral a) => a -> [a]
primesUpTo n = sieve [2..n]

sieve :: (Integral a) => [a] -> [a]
sieve [] = []
sieve (p:xs) = p : [x | x <- sieve xs, x `mod` p > 0]
