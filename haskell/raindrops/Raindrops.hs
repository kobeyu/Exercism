module Raindrops (convert) where
import qualified Data.Map.Strict as M
import Data.Monoid

rain :: M.Map Integer String
rain = M.fromAscList [(3,"Pling"),(5,"Plang"),(7,"Plong")]

ifEmptyThen :: (Monoid m,Eq m) => m -> m -> m
ifEmptyThen def x = if x == mempty then def else x

convert :: Integer -> String
convert n = ifEmptyThen (show n). concat . M.elems . M.filterWithKey (\k _ -> n `mod` k == 0) $ rain