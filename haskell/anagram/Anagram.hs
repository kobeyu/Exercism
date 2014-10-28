module Anagram (anagramsFor) where
import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor a = filter (and . sequence [(/=lowa),(==norma) . sort] . low)
	where 
		low = map toLower
		lowa = low a
		norma = sort lowa