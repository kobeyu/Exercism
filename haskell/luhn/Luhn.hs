module Luhn (checkDigit, addends, checksum, isValid, create) where
import Data.List(unfoldr)
import Data.Tuple(swap)
import Control.Arrow((&&&))

checkDigit :: Integer -> Integer
checkDigit  = (`mod` 10)

sub9 :: Integer -> Integer
sub9 n
	| n < 10 = n
	|otherwise = n - 9

explodeRev :: Integer -> [Integer]
explodeRev = unfoldr step
	where
		step :: Integer -> Maybe (Integer,Integer)
		step 0 = Nothing
		step r = Just . swap . (`divMod` 10) $ r

addends :: Integer -> [Integer]
addends = reverse . zipWith ($) (cycle [id,sub9 . (*2)]) . explodeRev
				
checksum :: Integer -> Integer
checksum = (`mod` 10) . sum . addends 

isValid :: Integer -> Bool
isValid = (==0) . checksum

create :: Integer -> Integer
create = uncurry (+) . (id &&& (`mod` 10) . negate . checksum) . (10*)
