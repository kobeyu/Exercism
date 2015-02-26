module Triangle (row, triangle) where

choose :: Integral a => a -> a -> a
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

row :: Integral a => a -> [a]
row n = map (choose (n-1)) [0..n-1]
--row = ((map . choose) <*> enumFromTo 0) . pred

triangle :: Integral a => [[a]]
triangle = map row [1..]
