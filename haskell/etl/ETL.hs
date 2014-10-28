module ETL (transform) where
import qualified Data.Map.Strict as M
import Data.Char(toLower)

transform :: M.Map Int [String] -> M.Map String Int
transform = M.fromList . concatMap go . M.toList
	where go (i,l) = [(map toLower s,i) | s <- l]
