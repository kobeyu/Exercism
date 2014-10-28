module Clock (fromHourMin, toString) where
import Text.Printf(printf)
import Data.Function(on)

newtype Clock = Clock {unClock:: Int}
	deriving (Eq,Show)

instance Num Clock where
	(+) = (fromIntegral .) . ((+) `on` unClock) 
	(*) = error "Can't multiply Clock by Clock"
	abs = id 
	signum = const 1 
	negate = fromIntegral . negate . unClock 
	fromInteger = Clock . fromIntegral . (`mod` 1440)

fromHourMin :: Int -> Int -> Clock
fromHourMin = (fromIntegral . ) . (+) . (*60)

toString :: Clock -> String
toString = uncurry (printf "%02d:%02d") . (`divMod` 60) . unClock
