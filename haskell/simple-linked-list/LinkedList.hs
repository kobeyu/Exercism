module LinkedList(new,isNil,nil,datum,next,toList,fromList,reverseLinkedList) where

import Prelude hiding (foldr)
import Data.Foldable(Foldable,foldr,foldl',foldMap,toList)
import Data.Monoid(mappend,mempty)

data List a = Nil | List {datum :: a, next :: List a} 

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (List x xs) = f x `mappend` foldMap f xs

new :: a -> List a -> List a
new = List

isNil :: List a -> Bool
isNil Nil = True
isNil _ = False

nil :: List a
nil = Nil

fromList :: [a] -> List a
fromList = foldr List Nil

reverseLinkedList :: List a -> List a
reverseLinkedList = foldl' (flip List) Nil
