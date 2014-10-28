{-# LANGUAGE BangPatterns #-}
module Octal (showOct, readOct) where
import Data.List(foldl')

showOct :: (Integral a, Show a) => a -> String
showOct = concatMap show . digits 8 []

readOct :: (Integral a) => String -> a
readOct = undigits 8 . map readChar

digits :: (Integral a) => a -> [a] -> a -> [a]
digits _ acc 0 = acc
digits b !acc a = let (d,r) = a `divMod` b in digits b (r:acc) d

undigits :: (Integral a) => a -> [a] -> a
undigits b = foldl' ((+) . (b*)) 0

readChar :: (Integral a) => Char -> a
readChar = fromIntegral . subtract (fromEnum '0') . fromEnum
