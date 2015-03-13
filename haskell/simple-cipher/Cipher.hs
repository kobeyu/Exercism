module Cipher(caesarEncode, caesarDecode, caesarEncodeRandom) where
import Control.Monad(replicateM)
import System.Random(randomRIO)
import Control.Arrow((&&&))
import Data.Functor((<$>))
import Data.Function(on)

c2i :: Char -> Int
c2i = subtract (fromEnum 'a') . fromEnum

i2c :: Int -> Char
i2c = toEnum . (fromEnum 'a' +) . (`mod` 26)

caesarEncode :: String -> String -> String
caesarEncode = (map i2c .) . zipWith ((+) `on` c2i) . cycle

caesarDecode :: String -> String -> String
caesarDecode = (map i2c .) . zipWith (subtract `on` c2i) . cycle

caesarEncodeRandom :: String -> IO (String,String)
caesarEncodeRandom s = (id&&&flip caesarEncode s) <$> replicateM 100 (randomRIO ('a','z'))

