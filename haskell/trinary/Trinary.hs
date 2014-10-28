{-# LANGUAGE BangPatterns #-}
module Trinary (showTri, readTri) where

showTri :: (Integral a, Show a) => a -> String
showTri = concatMap show . digits 3 []

readTri :: (Integral a) => String -> a
readTri = undigits 3 0 . map readChar

digits :: (Integral a) => a -> [a] -> a -> [a]
digits _ acc 0 = acc
digits b !acc a = let (d,r) = a `divMod` b in digits b (r:acc) d

undigits :: (Integral a) => a -> a -> [a] -> a
undigits _ acc [] = acc
undigits b !acc (a:as) = undigits b (acc * b + a) as

readChar :: (Integral a) => Char -> a
readChar c = case c of
	'0' -> 0
	'1' -> 1
	'2' -> 2
	_ -> error $ "Bad digit: " ++ [c]
