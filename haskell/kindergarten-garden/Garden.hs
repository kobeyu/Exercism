module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List.Split(chunksOf)
import Data.List(transpose,sort)

type Garden = M.Map String [Plant]

data Plant = Violets | Radishes | Clover | Grass
	deriving(Show,Eq,Ord)

--"ABCD\r\nDEFG" -> ["ABDE","CDFG"]
mangleString :: String -> [String]
mangleString = map concat . transpose . map (chunksOf 2) . lines

readPlant :: Char -> Plant
readPlant x = case x of
	'V' -> Violets
	'R' -> Radishes
	'C' -> Clover
	'G' -> Grass
	_ -> error $ "Unknown Plant: "++[x]

garden :: [String] -> String -> Garden
garden students = M.fromList . zip (sort students) . map (map readPlant) . mangleString 

defaultGarden :: String -> Garden
defaultGarden = garden (words "Alice Bob Charlie David Eve Fred Ginny Harriet Ileana Joseph Kincaid Larry")

lookupPlants :: String -> Garden -> [Plant]
lookupPlants = (fromMaybe [] .). M.lookup

