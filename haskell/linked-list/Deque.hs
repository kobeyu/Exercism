{-# LANGUAGE TupleSections #-}
module Deque (mkDeque, push, pop, shift, unshift) where
import Data.Sequence(Seq,(|>),(<|),empty,viewr,ViewR(..),ViewL(..),viewl)
import Control.Concurrent.STM(atomically)
import Control.Concurrent.STM.TMVar(TMVar,putTMVar,takeTMVar,newTMVarIO)
import Control.Applicative((<$>))

type Deque a = TMVar (Seq a)

mkDeque :: IO (Deque a)
mkDeque = newTMVarIO empty

alter :: TMVar a -> (a -> (a,b)) -> IO b
alter d f = atomically $ do
	(s',r) <- f <$> takeTMVar d 
	putTMVar d s'
	return r

push :: Deque a -> a -> IO ()
push d a = alter d ((,()) . (|>a))

unshift :: Deque a -> a -> IO ()
unshift d a = alter d ((,()) . (a<|))

pop :: Deque a -> IO (Maybe a)
pop d = alter d $ \s -> 
	case viewr s of 
		EmptyR -> (s,Nothing)
		(r :> i) -> (r,Just i)

shift :: Deque a -> IO (Maybe a)
shift d = alter d $ \s -> 
	case viewl s of
		EmptyL -> (s,Nothing)
		(i :< r) -> (r,Just i)
