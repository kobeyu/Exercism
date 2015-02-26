module Prime (nth) where
import Data.Numbers.Primes

nth :: Int -> Integer
nth = (primes !!) . pred
