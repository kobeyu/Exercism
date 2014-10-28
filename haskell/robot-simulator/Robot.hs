module Robot (Bearing(..), Robot, mkRobot,
              coordinates, simulate,
              bearing, turnRight, turnLeft)
			where
import Control.Arrow(first,second)
import Data.List(foldl')

data Bearing = North |East|South|West deriving (Show,Eq,Enum)

data Robot = Robot {bearing:: Bearing, coordinates::(Int,Int)}
	deriving (Show,Eq)

mkRobot :: Bearing -> (Int,Int) -> Robot
mkRobot = Robot

step ::  Robot -> Char -> Robot
step r 'L' = r {bearing = turnLeft (bearing r)}
step r 'R' = r {bearing = turnRight (bearing r)}
step r 'A' = r {coordinates = newPos (coordinates r)}
	where 
	newPos = case bearing r of
		North -> second succ 
		East -> first succ
		South -> second pred
		West -> first pred
step _ x = error $ "Unknown instruction: " ++ [x]


simulate :: Robot -> String -> Robot
simulate = foldl' step 

turn :: Int -> Bearing -> Bearing
turn i = toEnum . (`mod` 4) . (+i) . fromEnum

turnLeft :: Bearing -> Bearing
turnLeft = turn (-1)

turnRight :: Bearing -> Bearing
turnRight = turn 1