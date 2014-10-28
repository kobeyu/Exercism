module Scrabble (scoreLetter, scoreWord) where
import Data.Maybe(fromMaybe)
import Data.Char(toUpper)
import qualified Data.Map.Strict as M

swapexpand :: (a,[b]) -> [(b,a)]
swapexpand (a,bs) = [(b,a)|b <- bs]

scores :: M.Map Char Int
scores = M.fromList . concatMap swapexpand $ 
	[(1, "AEIOULNRST")
	,(2, "DG")
	,(3, "BCMP")
	,(4, "FHVWY")
	,(5, "K")
	,(8, "JX")
	,(10, "QZ")]

scoreLetter :: Char -> Int
scoreLetter = fromMaybe 0 . (`M.lookup` scores) . toUpper

scoreWord :: String -> Int
scoreWord = sum . map scoreLetter
