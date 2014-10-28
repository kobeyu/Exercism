module Meetup (Weekday(..), Schedule(..), meetupDay) where
import Data.Time.Calendar(Day,fromGregorian,gregorianMonthLength,addDays)
import Data.Time.Calendar.WeekDate(toWeekDate)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
	deriving (Show,Eq,Enum)

data Schedule = First | Second | Third | Fourth | Teenth | Last
	deriving (Show,Eq)

dayOfWeek :: Day -> Int
dayOfWeek = (\(_,_,i) -> i-1) . toWeekDate 

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay s wday y m = case s of
	First -> find 1
	Second -> find 8
	Third -> find 15
	Fourth -> find 22
	Teenth -> find 13
	Last -> find (gregorianMonthLength y m - 6)
	where 
		find :: Int -> Day
		find = findNext wday . fromGregorian y m

findNext :: Weekday -> Day -> Day
findNext wday d = addDays (fromIntegral diff) d
	where
		diff = (fromEnum wday - dayOfWeek d) `mod` 7

