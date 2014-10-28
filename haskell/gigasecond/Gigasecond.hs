module Gigasecond(fromDay) where
import Data.Time.Calendar (addDays,Day)

--here we ignore the existence of leap seconds, daylight savings times, and future
--governmental meddling with calendars.
fromDay :: Day -> Day
fromDay = addDays $ (10^(9::Integer)) `div` (60*60*24)