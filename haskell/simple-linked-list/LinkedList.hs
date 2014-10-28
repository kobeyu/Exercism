module LinkedList(new,isNil,nil,datum,next,toList,fromList,reverseLinkedList) where

data List a = Nil | List {datum :: a, next :: List a} 

new :: a -> List a -> List a
new = List
isNil :: List a -> Bool
isNil Nil = True
isNil _ = False

nil :: List a
nil = Nil
toList :: List a -> [a]
toList Nil = []
toList (List a as) = a : toList as
fromList :: [a] -> List a
fromList = foldr List Nil

reverseLinkedList :: List a -> List a
reverseLinkedList = go Nil
	where
		go l Nil = l
		go l (List a b) = go (List a l) b
