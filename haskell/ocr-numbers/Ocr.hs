module OCR (convert) where
import Data.List.Split(chunksOf)
import Data.List(transpose,intercalate)
import Data.Maybe(fromMaybe)

digits :: [(String,Char)]
digits = flip zip ['0'..'9'] . split $
 [" _     _  _     _  _  _  _  _ "
 ,"| |  | _| _||_||_ |_   ||_||_|"
 ,"|_|  ||_  _|  | _||_|  ||_| _|"
 ,"                              "]

convert :: String -> String
convert = intercalate "," . map convertLine . chunksOf 4 . lines

split :: [String] -> [String]
split = map unlines . transpose . map (chunksOf 3)

convertLine :: [String] -> String
convertLine = map convert1 . split

convert1 :: String -> Char
convert1 = fromMaybe '?' . (`lookup` digits)

