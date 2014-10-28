module Main where
import Hexadecimal(hexToInt)

main :: IO ()
main = do
	s <- getLine
	print $ hexToInt s