module PigLatin(translate) where
import Data.Tuple(swap)

translate :: String -> String
translate = unwords . map ((++"ay") . qu . tr) . words

qu :: String -> String
qu ('u':xs) | last xs == 'q' = xs ++ "u"
qu x = x

tr :: String -> String
tr = uncurry (++) . swap . break (`elem` "aeiou")
