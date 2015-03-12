module Sieve (primesUpTo) where
import Control.Monad(filterM)
import Data.STRef
import Control.Loop
import qualified Data.Vector as V(elemIndices,create,toList,findIndex,freeze)
import qualified Data.Vector.Mutable as MV(replicate,write,slice,read)

primesUpTo ::  Int -> [Int]
primesUpTo n = V.toList . V.elemIndices True $ V.create $ do
    mv <- MV.replicate (n+1) True
    let clear = mapM_ $ flip (MV.write mv) False
    clear [0..1]
    let markNonPrimesFrom p = do
        primeCandidates <- filterM (MV.read mv) [p..n]
        case primeCandidates of
            [] -> return ()
            (x:_) -> do
                clear [2*x,3*x..n]
                markNonPrimesFrom (x+1)
    markNonPrimesFrom 2
    return mv

