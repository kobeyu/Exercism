module Roman (numerals) where
import qualified Data.Map.Strict as M
import Data.List(unfoldr)
import Control.Applicative((<$>))

vals :: M.Map Int String
vals = M.fromAscList 
	[ (1,"I")
	, (4,"IV")
	, (5,"V")
	, (9,"IX")
	, (10,"X")
	, (40, "XL")
	, (50,"L")
	, (90, "XC")
	, (100,"C")
	, (400, "CD")
	, (500,"D")
	, (900, "CM")
	, (1000, "M")]

numerals :: Int -> String
numerals = concat . unfoldr step 
	where 
		step i = tuple i <$> (i `M.lookupLE` vals)
		tuple i (k,v) = (v,i-k)

