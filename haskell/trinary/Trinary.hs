{-# LANGUAGE BangPatterns #-}
module Trinary (showTri, readTri) where
import Data.List(foldl')

showTri :: (Integral a, Show a) => a -> String
showTri = concatMap show . digits 3 []

readTri :: (Integral a) => String -> a
readTri = undigits 3 . map readChar

digits :: (Integral a) => a -> [a] -> a -> [a]
digits _ acc 0 = acc
digits b !acc a = let (d,r) = a `divMod` b in digits b (r:acc) d

undigits :: (Integral a) => a -> [a] -> a
undigits b = foldl' ((+) . (b*)) 0

readChar :: (Integral a) => Char -> a
readChar = fromIntegral . subtract (fromEnum '0') . fromEnum
