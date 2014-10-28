module Hexadecimal (hexToInt) where
import Data.Char(ord)
import Data.Maybe(fromMaybe)
import Control.Monad(foldM)
import Data.Ix

charVal :: Char -> Maybe Int
charVal c 
	| inRange ('0','9') c = Just $      ord c - ord '0'
	| inRange ('a','f') c = Just $ 10 + ord c - ord 'a'
	| inRange ('A','F') c = Just $ 10 + ord c - ord 'F'
	| otherwise = Nothing

hexToInt :: String -> Int
hexToInt = fromMaybe 0 . foldM f 0
	where 
		f a = a `seq` (fmap (16 * a +) . charVal)

