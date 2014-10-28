module Grains (square, total) where

square :: Integer -> Integer
square = (2^) . (-1+)
--or square x = 2^(x-1)

total :: Integer
total = sum $ map square [1..64]
