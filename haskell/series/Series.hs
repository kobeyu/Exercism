module Series(slices) where

lengthIsAtLeast :: Int -> [a] -> Bool
lengthIsAtLeast t = (==t) . length . take t

readChar :: (Integral a) => Char -> a
readChar = fromIntegral . subtract (fromEnum '0') . fromEnum

slices :: Integral a => Int -> String -> [[a]]
slices n xs
    | lengthIsAtLeast n xs = map readChar (take n xs) : slices n (tail xs)
    | otherwise = []

