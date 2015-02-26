module PigLatin(translate) where

translate :: String -> String
translate = unwords . map tr . words

tr :: String -> String
tr ('q':'u':xs) = xs ++ "quay"
tr (x:xs)
   | x `elem` "aeiou" = (x:xs) ++ "ay"
   | otherwise = tr (xs ++ [x])
