module CustomSet (CustomSet,fromList,empty,delete,difference,isDisjointFrom,
	null,intersection,member,insert,size,isSubsetOf,toList,union) where
import           Prelude hiding (null)
import Data.Monoid(mempty,(<>))
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Function (on)

data CustomSet a = Bt {node :: a, _sz :: Int, left,right::CustomSet a} 
                 | Leaf

instance Show a => Show (CustomSet a) where
	show = ("fromList "++) . show . toList

instance Eq a => Eq (CustomSet a) where
	(==) = (==) `on` toList

instance F.Foldable CustomSet where
	foldMap _ Leaf = mempty
	foldMap f (Bt a _ l r) = F.foldMap f l <> f a <> F.foldMap f r

data Balance = Ok | LeftHeavy | RightHeavy deriving(Show)

bt :: (Show a,Ord a) => a -> CustomSet a -> CustomSet a -> CustomSet a
bt a l r = case balance t of
	Ok -> t
	LeftHeavy -> bt (node l) (left l) (insert a . union r .  right $ l)
	RightHeavy -> bt (node r) (insert a . union l . left $ r) (right r)
	where 
		t = Bt a (size l + size r + 1) l r

balance :: CustomSet a -> Balance
balance t 
	| sl * 2 < sr = RightHeavy
	| sr * 2 < sl = LeftHeavy
	| otherwise = Ok
	where 
		sl = 1 + size (left t)
		sr = 1 + size (right t)

toList :: CustomSet a -> [a]
toList = F.toList

fromList :: (Show a, Ord a) => [a] -> CustomSet a
fromList = L.foldl' (flip insert) empty

empty :: CustomSet a
empty = Leaf

delete :: (Show a,Ord a) => a -> CustomSet a -> CustomSet a
delete i (Bt a _ l r) = case compare i a of
	LT -> bt a (delete i l) r 
	GT -> bt a l (delete i r)
	EQ -> l `union` r
delete _ t = t

member :: Ord a => a -> CustomSet a -> Bool
member _ Leaf = False
member i (Bt a _ l r) = case compare i a of
	LT -> member i l
	GT -> member i r
	EQ -> True

insert :: (Show a,Ord a) => a -> CustomSet a -> CustomSet a
insert i Leaf = Bt i 1 Leaf Leaf
insert i t@(Bt a _ l r) = case compare i a of
	LT -> bt a (insert i l) r 
	GT -> bt a l (insert i r)
	EQ -> t

difference :: (Show a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference a b = foldr delete a (toList b)

isDisjointFrom :: (Show a, Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom x y = null (x `intersection` y) 

null :: CustomSet a -> Bool
null Leaf = True
null _ = False

intersection :: (Show a, Ord a)=> CustomSet a -> CustomSet a -> CustomSet a
intersection a = F.foldl' possiblyInsert empty
	where
		possiblyInsert x y 
			| y `member` a = insert y x
			| otherwise = x

size :: CustomSet a -> Int
size Leaf = 0
size (Bt _ s _ _) = s

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf xs ys = F.all (`member` ys) xs

union :: (Show a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union = F.foldl' (flip insert)
