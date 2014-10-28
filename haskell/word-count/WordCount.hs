module WordCount (wordCount) where
import qualified Data.Map.Strict as M(Map,fromListWith)
import Data.Char (toLower,isAlphaNum)
import Data.List.Split (wordsBy)

wordCount :: String -> M.Map String Int
wordCount = M.fromListWith (+) . flip zip (repeat 1) . wordsBy (not . isAlphaNum) . map toLower
