{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Deque (mkDeque, push, pop, shift, unshift) where
import           Control.Applicative         ((<$>),(<*>))
import           Control.Concurrent.STM      (atomically,STM)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import Control.Monad((<=<),(>=>))

data Circle a = Node {this :: a, prev,next :: TVar(Circle a)}

instance Eq (Circle a) where
	(Node _ p n) == (Node _ p' n') = (p,n) == (p',n')

mkCircle :: a -> STM (Circle a)
mkCircle a = mdo
	i <- Node a <$> newTVar i <*> newTVar i
	return i

insert :: a -> Circle a -> STM (Circle a)
insert a nextItem = do
	prevItem <- readTVar (prev nextItem)
	newItem <- Node a <$> newTVar prevItem <*> newTVar nextItem
	writeTVar (prev nextItem) newItem
	writeTVar (next prevItem) newItem
	return newItem

delete :: Circle a -> STM (Maybe a, Maybe (Circle a))
delete c = do
	nextItem <- readTVar (next c)
	if nextItem == c then 
		return (Just (this c),Nothing)
	else do
		prevItem <- readTVar (prev c)
		writeTVar (next prevItem) nextItem
		writeTVar (prev nextItem) prevItem
		return (Just (this c), Just nextItem)


newtype Deque a = MkDeque (TVar (Maybe (Circle a)))

mkDeque :: IO (Deque a)
mkDeque = atomically $ MkDeque <$> newTVar Nothing 

pushnshift :: (Circle a -> STM (Circle a)) -> Deque a -> a -> IO ()
pushnshift f (MkDeque l) a = atomically $ 
	readTVar l >>=
	maybe (mkCircle a) (insert a >=> f) >>= writeTVar l . Just

unshift :: Deque a -> a -> IO ()
unshift = pushnshift return

push :: Deque a -> a -> IO ()
push = pushnshift (readTVar . next)

popshift :: (Circle a -> STM (Circle a)) -> Deque a -> IO (Maybe a)
popshift f (MkDeque l) = atomically $ do
    (r,n) <- readTVar l >>= maybe (return (Nothing,Nothing)) (delete <=< f)
    writeTVar l n >> return r

shift :: Deque a -> IO (Maybe a)
shift = popshift return 

pop :: Deque a -> IO (Maybe a)
pop = popshift (readTVar . prev)
