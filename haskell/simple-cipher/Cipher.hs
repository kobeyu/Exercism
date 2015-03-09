module Cipher(caesarEncode, caesarDecode, caesarEncodeRandom) where

f :: Char -> Char -> Char
f k v = toEnum (fromEnum 'a' + ((fromEnum v + fromEnum k) `mod` 26))

caesarEncode :: String -> String -> String
caesarEncode key text = zipWith f (cycle key) text

caesarDecode :: String -> String -> String
caesarDecode = undefined

caesarEncodeRandom :: String -> IO (String,String)
caesarEncodeRandom = undefined

