module Counting(Color(..), territories, territoryFor) where
import Safe(headMay)
import qualified Data.Set as S
import qualified Data.Map as M

data Color = Black | White deriving (Eq, Ord, Show) -- More derivations are allowed
type Coord = (Int, Int)

data Board = Board { black,white,blank :: S.Set Coord }

readColor :: Char -> Maybe Color
readColor 'B' = Just Black
readColor 'W' = Just White
readColor _ = Nothing

mkBoard :: [[Char]] -> Board
mkBoard c = S.
    where m = M.fromListWith (++) [(readColor e,[(x,y)]) | (x,r) <- zip [1..] c, (y,e) <- zip [1..] r]

neighbours :: Coord -> [Coord]
neighbours (x,y) = [(x-1,y),(x,y-1),(x,y+1),(x+1,y)]

--
-- Your code should define the following functions:
--
--
--   Returns the coordinates (1 based, top left is (1,1)) of of the points
--   in each territory along with who "owns" the territory. A territory is
--   owned by one of the players if that player's stones are the only
--   stones adjacent to the territory.
--
territories :: [[Char]] -> [(Set Coord, Maybe Color)]
territories = f S.empty . mkBoard
    where
        f seen =


--   Returns the territory that contains the coordinate along with the
--   owner of the territory. If the coordinate does not point to an empty
--   location returns Nothing.
territoryFor :: [[Char]] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor b c = headMay . filter (member c . fst) . territories $ b


