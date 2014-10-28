module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault = sumOfMultiples [3,5]

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples mults m = sum [ c | c <-[1..(m-1)], any ((==0) . (c `mod`)) mults]
