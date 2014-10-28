module Binary (toDecimal) where
import Data.Bits
import Data.List(foldl')

safeRead :: Char -> Integer
safeRead x = if x == '1' then 1 else 0

toDecimal :: String -> Integer
toDecimal = foldl' ((.|.) . (`shift` 1)) 0 . map safeRead
