module Frequency(frequency) where
import qualified Data.Map as M(Map,unionsWith,insertWith,empty)
import qualified Data.Text as T(concat,toLower,filter,foldl',Text)
import Data.Char(isLetter)
import Data.List(transpose)
import Data.List.Split(chunksOf)
import Control.Parallel.Strategies(parMap, rdeepseq)

work :: [T.Text] -> M.Map Char Int
work = T.foldl' (\m c -> M.insertWith (+) c 1 m) M.empty . T.filter isLetter . T.toLower . T.concat

nChunks :: Int -> [a] -> [[a]]
nChunks = (transpose . ) . chunksOf

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency x = M.unionsWith (+) . parMap rdeepseq work . nChunks x
