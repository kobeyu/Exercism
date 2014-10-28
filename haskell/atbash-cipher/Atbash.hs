module Atbash (encode) where
import Data.Char(toLower,isAlphaNum,chr,ord,isAsciiLower)
import Control.Applicative (liftA3)
import Data.Bool(bool)
import Data.List.Split(chunksOf)

flipChar :: Char -> Char
flipChar = liftA3 bool id f isAsciiLower
	where 
		f = chr . ((ord 'a' + ord 'z') -) . ord

encode :: String -> String
encode = unwords . chunksOf 5 . map (flipChar . toLower) . filter isAlphaNum
