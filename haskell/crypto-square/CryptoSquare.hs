module CryptoSquare (normalizePlaintext,
                     squareSize,
                     plaintextSegments,
                     ciphertext,
                     normalizeCiphertext) where
import Data.Char(toLower,isAlphaNum)
import Data.List.Split(chunksOf)
import Data.List(transpose,genericLength)

squareSize :: String -> Int
squareSize = ceiling . (sqrt::Double->Double) . genericLength

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isAlphaNum

plaintextSegments :: String -> [String]
plaintextSegments = ( squareSize >>= chunksOf ) . normalizePlaintext 

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . chunksOf 5 . ciphertext
{- with most methods inlined, that's:
	normalizeCiphertext = 
		unwords . 
		chunksOf 5 .
		concat . 
		transpose . 
		(squareSize >>= chunksOf) . 
		map toLower . 
		filter isAlphaNum -}

