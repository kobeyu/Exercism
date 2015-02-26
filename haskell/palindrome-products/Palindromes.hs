{-# LANGUAGE BangPatterns #-}
module Palindromes(largestPalindrome,smallestPalindrome) where
import Control.Monad (ap)
import Data.Function (on)
import Data.List (groupBy,foldl',genericLength)

type PalinResult a = (a,[(a,a)])

digits :: (Integral a) => a -> [a] -> a -> [a]
digits _ acc 0 = acc
digits b !acc a = let (d,r) = a `divMod` b in digits b (r:acc) d

undigits :: (Integral a) => a -> [a] -> a
undigits b = foldl' ((+) . (b*)) 0

data Dir = Asc | Desc deriving (Eq)

palindromesOfLength :: (Integral a) => Dir -> a -> [a]
palindromesOfLength Asc 1 = [0..9]
palindromesOfLength Desc 1 = [9,8..0]
palindromesOfLength dir n
    | m == 0 = map makeEven ds
    | m == 1 = concatMap makeOdds ds
    where
        (h,m) = n `divMod` 2
        order = (if dir == Asc then id else reverse)
        ds = order [10^(h-1)..(10^h) - 1]
        makeEven :: Integral a => a -> a
        makeEven = (undigits 10) . (\x -> x ++ reverse x) . digits 10 []
        makeOdds :: Integral a => a -> [a]
        makeOdds = (\s -> [undigits 10 $ s ++ [mid] ++ reverse s | mid <- order [0..9]]) . digits 10 []

palindromes :: (Integral a) => Dir -> a -> a -> [a]
palindromes dir min max = filter inRange $ concatMap (palindromesOfLength dir) digitCounts
    where
        inRange x = x >= min && x <= max
        order = if dir == Asc then id else reverse
        dLen = genericLength . digits 10 []
        digitCounts = order [dLen min .. dLen max]

factorsBetween :: (Integral a) => a -> a -> a -> [(a,a)]
factorsBetween x l h = [(a,d) | a <- [l..h], let (d,m) = x `divMod` a, d >= a, d <= h, m == 0]

largestPalindrome :: Integral a => a -> a -> PalinResult a
largestPalindrome l h = head [(x,f) | x <- palindromes Desc (l * l) (h * h), let f = factorsBetween x l h, f /= []]

smallestPalindrome :: Integral a => a -> a -> PalinResult a
smallestPalindrome l h = head [(x,f) | x <- palindromes Asc (l*l) (h*h), let f = factorsBetween x l h, f /= []]
