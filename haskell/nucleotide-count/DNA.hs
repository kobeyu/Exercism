module DNA (count, nucleotideCounts) where
import qualified Data.Map.Strict as M (Map,fromList,adjust,lookup)
import Data.Maybe(fromMaybe)

count :: Char -> String -> Integer
count c = fromMaybe (error $ "invalid nucleotide '"++[c]++"'") . M.lookup c . nucleotideCounts

nucleotideCounts :: String -> M.Map Char Integer
nucleotideCounts = run (M.fromList [('A',0),('C',0),('G',0),('T',0)])
	where 
		run = foldr (M.adjust (+1))
