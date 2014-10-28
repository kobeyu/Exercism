module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

--copied this one. Correct use of `seq` is beyond me...
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = go 
  where go z [] = z
        go z (x:xs) = let z' = f z x in z' `seq` go z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x $ foldr f z xs

length :: [a] -> Int
length = foldr (const (+1)) 0

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (a:as)
  | f a = a : filter f as
  | otherwise = filter f as

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat = foldr (++) []
