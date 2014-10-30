{-# LANGUAGE TupleSections #-}
module Deque (mkDeque, push, pop, shift, unshift) where
import Control.Concurrent.STM(atomically,STM)
import Control.Concurrent.STM.TMVar(TMVar,readTMVar,swapTMVar,newTMVar,newEmptyTMVar,putTMVar)
import Control.Monad(void)

data Deque a = MkDeque {front,back :: TMVar (DoubleList a)}

data DoubleList a = EndL {next :: TMVar (DoubleList a)} 
                  | EndR {prev :: TMVar (DoubleList a)}
                  | Cons {_this :: a, prev,next :: TMVar (DoubleList a)}

mkDeque :: IO (Deque a)
mkDeque = atomically $ do 
	t1 <- newEmptyTMVar
	t2 <- newEmptyTMVar
	let left = EndL t2
	let	right = EndR t1
	putTMVar t1 left
	putTMVar t2 right
	f <- newTMVar left
	b <- newTMVar right
	return $ MkDeque f b

insert :: DoubleList a -> a -> STM ()
insert nextItem a = do
	prevItem <- readTMVar (prev nextItem)
	prevPtr <- newTMVar prevItem
	nextPtr <- newTMVar nextItem
	let newItem = Cons a prevPtr nextPtr
	void $ swapTMVar (prev nextItem) newItem
	void $ swapTMVar (next prevItem) newItem

delete :: DoubleList a -> STM ()
delete t = do
	prevItem <- readTMVar (prev t)
	nextItem <- readTMVar (next t)
	void $ swapTMVar (prev nextItem) prevItem
	void $ swapTMVar (next prevItem) nextItem

unshift :: Deque a -> a -> IO ()
unshift d a = atomically $ do
	marker <- readTMVar (front d)
	oldHead <- readTMVar (next marker)
	insert oldHead a

push :: Deque a -> a -> IO ()
push d a = atomically $ do
	marker <- readTMVar (back d)
	insert marker a

deleteCons :: DoubleList a -> STM (Maybe a)
deleteCons l = case l of 
	Cons t _ _ -> delete l >> return (Just t)
	_          -> return Nothing

pop :: Deque a -> IO (Maybe a)
pop d = atomically $ do
	e <- readTMVar (back d)
	readTMVar (prev e) >>= deleteCons

shift :: Deque a -> IO (Maybe a)
shift d = atomically $ do
	e <- readTMVar (front d)
	readTMVar (next e) >>= deleteCons
