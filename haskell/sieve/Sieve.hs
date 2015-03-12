module Sieve (primesUpTo) where
import Control.Monad(forM_,when)
import qualified Data.Vector as V(elemIndices,create,toList)
import qualified Data.Vector.Mutable as MV(replicate,write,read)

primesUpTo ::  Int -> [Int]
primesUpTo n = V.toList . V.elemIndices True $ V.create $ do
    mv <- MV.replicate (n+1) True
    let clear = mapM_ $ flip (MV.write mv) False
    clear [0..1]
    forM_ [2..n] $ \i -> MV.read mv i >>= flip when (clear [2*i,3*i..n])
    return mv

