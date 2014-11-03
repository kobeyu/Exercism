module School(School,add,empty,grade,sorted) where
import qualified Data.Map.Strict as M (Map, empty, insertWith, lookup, map,
                                       singleton, toList, unionWith)
import           Data.Maybe      (fromMaybe)

type School = M.Map Int (M.Map String Int)

add :: Int -> String -> School -> School
add g = M.insertWith (M.unionWith (+)) g . (`M.singleton` 1)

empty :: School
empty = M.empty

expand :: M.Map String Int -> [String]
expand = concatMap (uncurry $ flip replicate) . M.toList

sorted :: School -> [(Int,[String])]
sorted = M.toList . M.map expand

grade :: Int -> School -> [String]
grade g = fromMaybe [] . M.lookup g . M.map expand
