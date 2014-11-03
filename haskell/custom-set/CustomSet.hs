module CustomSet (CustomSet,fromList,empty,delete,difference,isDisjointFrom,
	null,intersection,member,insert,size,isSubsetOf,toList,union) where
import           Prelude hiding (null)
import qualified Data.List as L
import Data.List ((\\))

data CustomSet a = CSet {toList :: [a]}
	deriving (Show)

instance Eq a => Eq (CustomSet a) where
	(CSet a) == (CSet b) = a == b

fromList :: [a] -> CustomSet a
fromList = CSet

empty :: CustomSet a
empty = CSet []

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete a (CSet xs) = CSet (a `L.delete` xs)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CSet xs) (CSet ys) = CSet (xs \\ ys)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom x y = null (x `intersection` y) 

null :: CustomSet a -> Bool
null (CSet []) = True
null _ = False

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection = undefined

member :: a -> CustomSet a -> Bool
member = undefined

insert :: a -> CustomSet a -> CustomSet a
insert = undefined

size :: CustomSet a -> Int
size = undefined

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf xs ys = intersection xs ys == xs

union :: CustomSet a -> CustomSet a -> CustomSet a
union = undefined
