module School(School,add,empty,grade,sorted) where
import qualified Data.Map.Strict as M(Map,insertWith,empty,toList,map,lookup)
import qualified Data.Set as S(Set,union,singleton,toList)
import Data.Maybe(fromMaybe)

type School = M.Map Int (S.Set String)

add :: Int -> String -> School -> School
add g = M.insertWith S.union g . S.singleton 
empty :: School
empty = M.empty
sorted :: School -> [(Int,[String])]
sorted = M.toList . M.map S.toList
grade :: Int -> School -> [String]
grade g = fromMaybe [] . M.lookup g . M.map S.toList