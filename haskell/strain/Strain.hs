module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (a:as)
	| p a = a : keep p as
	| otherwise = keep p as

discard :: (a -> Bool) -> [a] -> [a]
discard f = keep (not . f)