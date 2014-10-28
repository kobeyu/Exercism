module Beer (sing, verse) where
import Data.Char(toUpper)

sing :: Int -> Int -> String
sing a b = unlines $ map verse [a,(a-1)..b]

bottle :: Int -> String
bottle 0 = "no more bottles of beer"
bottle 1 = "1 bottle of beer"
bottle n = show n ++ " bottles of beer"

thirdLine :: Int -> String
thirdLine 0 = "Go to the store and buy some more, "
thirdLine n = concat ["Take ",x," down and pass it around, "]
	where x = if n == 1 then "it" else "one"

verse :: Int -> String
verse = concat . sequence 
	[ initCase . bottle, const " on the wall, "
	, bottle, const ".\n"
	, thirdLine
	, bottle . next, const " on the wall.\n"]
	where
		next n = (n - 1) `mod` 100
		initCase s = toUpper (head s) : tail s
