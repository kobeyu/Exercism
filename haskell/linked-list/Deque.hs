{-# LANGUAGE RecursiveDo #-}
module Deque (mkDeque, push, pop, shift, unshift) where
import Control.Concurrent.STM(atomically,STM)
import Control.Concurrent.STM.TVar(TVar,newTVar,readTVar,writeTVar)
import Control.Monad(liftM2)

data Deque a = EndLR {next,prev :: TVar (Deque a)} 
             | Cons {_this :: a, prev,next :: TVar (Deque a)}

mkDeque :: IO (Deque a)
mkDeque = atomically $ mdo 
    let m = EndLR t1 t2 
    t1 <- newTVar m
    t2 <- newTVar m
    return m

insert :: a -> Deque a -> STM ()
insert a nextItem = do
	prevItem <- readTVar (prev nextItem)
	newItem <- liftM2 (Cons a) (newTVar prevItem) (newTVar nextItem)
	writeTVar (prev nextItem) newItem
	writeTVar (next prevItem) newItem

delete :: Deque a -> STM ()
delete t = do
	prevItem <- readTVar (prev t)
	nextItem <- readTVar (next t)
	writeTVar (prev nextItem) prevItem
	writeTVar (next prevItem) nextItem

unshift :: Deque a -> a -> IO ()
unshift d a = atomically $ readTVar (next d) >>= insert a

push :: Deque a -> a -> IO ()
push d a = atomically $ insert a d

deleteCons :: Deque a -> STM (Maybe a)
deleteCons l = case l of 
	Cons t _ _ -> delete l >> return (Just t)
	_          -> return Nothing

pop :: Deque a -> IO (Maybe a)
pop d = atomically $ readTVar (prev d) >>= deleteCons

shift :: Deque a -> IO (Maybe a)
shift d = atomically $ readTVar (next d) >>= deleteCons
