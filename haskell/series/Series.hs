module Series(slices) where

lengthIsAtLeast :: Int -> [a] -> Bool
lengthIsAtLeast n xs
    | n <= 0 = True
    | null xs = False
    | otherwise = lengthIsAtLeast (n-1) (tail xs)

readChar :: (Integral a) => Char -> a
readChar = fromIntegral . subtract (fromEnum '0') . fromEnum

slices :: Integral a => Int -> String -> [[a]]
slices n xs
    | lengthIsAtLeast n xs = map readChar (take n xs) : slices n (tail xs)
    | otherwise = []

