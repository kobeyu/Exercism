{-# LANGUAGE RecursiveDo #-}
module Deque (mkDeque, push, pop, shift, unshift) where
import Control.Concurrent.STM(atomically,STM)
import Control.Concurrent.STM.TVar

data Deque a = MkDeque {front,back :: DoubleList a}

data DoubleList a = EndL {next :: TVar (DoubleList a)} 
                  | EndR {prev :: TVar (DoubleList a)}
                  | Cons {_this :: a, prev,next :: TVar (DoubleList a)}

mkDeque :: IO (Deque a)
mkDeque = atomically $ mdo 
    t1 <- newTVar (EndL t2)
    t2 <- newTVar (EndR t1)
    return $ MkDeque (EndL t2) (EndR t1)

insert :: a -> DoubleList a -> STM ()
insert a nextItem = do
	prevItem <- readTVar (prev nextItem)
	prevPtr <- newTVar prevItem
	nextPtr <- newTVar nextItem
	let newItem = Cons a prevPtr nextPtr
	writeTVar (prev nextItem) newItem
	writeTVar (next prevItem) newItem

delete :: DoubleList a -> STM ()
delete t = do
	prevItem <- readTVar (prev t)
	nextItem <- readTVar (next t)
	writeTVar (prev nextItem) prevItem
	writeTVar (next prevItem) nextItem

unshift :: Deque a -> a -> IO ()
unshift d a = atomically $ readTVar (next (front d)) >>= insert a

push :: Deque a -> a -> IO ()
push d a = atomically $ insert a (back d)

deleteCons :: DoubleList a -> STM (Maybe a)
deleteCons l = case l of 
	Cons t _ _ -> delete l >> return (Just t)
	_          -> return Nothing

pop :: Deque a -> IO (Maybe a)
pop d = atomically $ readTVar (prev (back d)) >>= deleteCons

shift :: Deque a -> IO (Maybe a)
shift d = atomically $ readTVar (next (front d)) >>= deleteCons
