{-# LANGUAGE BangPatterns,ScopedTypeVariables #-}
module Palindromes(largestPalindrome,smallestPalindrome) where
import Control.Monad (ap)
import Data.Function (on)
import Data.List (groupBy,foldl')


type PalinResult a = (a,[(a,a)])

digits :: (Integral a) => a -> [a] -> a -> [a]
digits _ acc 0 = acc
digits b !acc a = let (d,r) = a `divMod` b in digits b (r:acc) d

undigits :: (Integral a) => a -> [a] -> a
undigits b = foldl' ((+) . (b*)) 0

palindromesTo :: (Integral a) => a -> [a]
palindromesTo x = [p | ds <- digitSets, let p = (undigits 10 $ ds ++ (reverse ds)), p <= x]
    where
      n = succ $ length $ digits 10 [] x
      digitSets = map (digits 10 []) $ [1..10^(n`div`2)]

isPalindrome :: (Integral a) => a -> Bool
isPalindrome = ap (==) reverse .  digits 10 []

factorsBetween :: (Integral a) => a -> a -> a -> [(a,a)]
factorsBetween x l h = [(a,d) | a <- [l..h], let (d,m) = x `divMod` a, d >= a, d <= h, m == 0]

palindromes :: Integral a => a -> a -> [PalinResult a]
palindromes a b =  [(x,factorsBetween x a b) | a1 <- [a..b], a2 <- [a1..b], let x = a1 * a2, isPalindrome x]

largestPalindrome :: Integral a => a -> a -> PalinResult a
largestPalindrome a b = head [(x,factorsBetween x a b) | a1 <- [b,b-1..a], a2 <- [b,b-1..a1], let x = a1 * a2, isPalindrome x]

smallestPalindrome :: Integral a => a -> a -> PalinResult a
smallestPalindrome = (head . ) . palindromes
