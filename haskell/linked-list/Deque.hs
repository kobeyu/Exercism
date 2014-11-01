module Deque (mkDeque, push, pop, shift, unshift) where
import           Control.Applicative         ((<$>))
import           Control.Concurrent.STM      (STM, atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Monad               (liftM2)
import           Data.Maybe                  (fromMaybe)
import           Data.Tuple                  (swap)

type Ref a = TVar (Maybe (DoubleList a))
--In these tuples, fst is start/next, and snd is end/prev.
type Deque a = (Ref a,Ref a)
data DoubleList a = Cons {_this :: a, links :: (Ref a,Ref a)}

mkDeque :: IO (Deque a)
mkDeque = atomically $ liftM2 (,) (newTVar Nothing) (newTVar Nothing)

type Direction a = (Ref a,Ref a) -> (Ref a,Ref a)

insert :: Direction a -> Deque a -> a -> STM ()
insert dir d a = do
	let (start,end) = dir d
	maybeNextItem <- readTVar start
	newItem <- (Cons a . dir) <$> liftM2 (,) (newTVar maybeNextItem) (newTVar Nothing)
	let backRef = fromMaybe end (snd . dir . links <$> maybeNextItem)
	writeTVar backRef (Just newItem)
	writeTVar start (Just newItem)

delete :: Direction a -> Deque a -> STM (Maybe a)
delete dir d = do
	let (start,end) = dir d
	maybeToRemove <- readTVar start
	case maybeToRemove of
		Nothing -> return Nothing
		Just (Cons i l) -> do
			newEnd <- readTVar (fst . dir $ l)
			let backRef = fromMaybe end (snd . dir . links <$> newEnd)
			writeTVar start newEnd
			writeTVar backRef newEnd
			return (Just i)

unshift :: Deque a -> a -> IO ()
unshift = (atomically .) . insert id

push :: Deque a -> a -> IO ()
push = (atomically .) . insert swap

shift :: Deque a -> IO (Maybe a)
shift = atomically . delete id

pop :: Deque a -> IO (Maybe a)
pop = atomically . delete swap
