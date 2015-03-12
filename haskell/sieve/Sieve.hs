module Sieve (primesUpTo) where
import qualified Data.Vector as V(elemIndices,create,toList)
import qualified Data.Vector.Mutable as MV(replicate,write)

primesUpTo ::  Int -> [Int]
primesUpTo n = V.toList . V.elemIndices True $ V.create $ do
    mv <- MV.replicate (n+1) True
    let clear = mapM_ $ flip (MV.write mv) False
    clear [0..1]
    mapM_ (\i -> clear [2*i,3*i..n]) [2..n]
    return mv

