module BST (bstLeft, bstRight, bstValue, singleton, insert, fromList, toList) where
import Data.List(partition)
import Data.Maybe(fromMaybe)
import Data.Foldable(toList,Foldable,foldMap)
import Data.Monoid(mappend,mempty)

data Bst a = Bst {bstLeft:: Maybe (Bst a), bstRight :: Maybe (Bst a), bstValue :: a}
	deriving (Show)

instance Foldable Bst where
	foldMap f (Bst l r v) = 
		maybe mempty (foldMap f) l `mappend` f v `mappend` maybe mempty (foldMap f) r

singleton :: a -> Bst a
singleton = Bst Nothing Nothing

insert :: Ord a => a -> Bst a -> Bst a
insert a (Bst l r v) 
	| a <= v = Bst (i l) r v
	| otherwise = Bst l (i r) v
	where i t = Just $ maybe (singleton a) (insert a) t

fromList :: Ord a => [a] -> Bst a
fromList = fromMaybe (error "Can't BST Empty List") . fromList'

fromList' :: Ord a => [a] -> Maybe (Bst a)
fromList' [] = Nothing
fromList' (x:xs) = Just $ Bst (fromList' lte) (fromList' gt) x
	where
		(lte,gt) = partition (<=x) xs
