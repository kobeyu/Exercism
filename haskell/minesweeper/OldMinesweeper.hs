{-# LANGUAGE ScopedTypeVariables #-}
module Minesweeper (annotate) where
import Data.Char(intToDigit)
import Data.List(transpose)

annotate :: [String] -> [String]
annotate = (map . map) (uncurry write) . annotateWithNeighbours ' '

shiftleft :: [[a]] -> [[a]]
shiftleft = map tail
shiftright :: a -> [[a]] -> [[a]]
shiftright b = map ((b:) . init)
shiftup :: [[a]] -> [[a]]
shiftup = tail
shiftdown :: a -> [[a]] -> [[a]]
shiftdown b m = replicate (length $ head m) b : init m

allShifts ::a ->  [[[a]] -> [[a]]]
allShifts b = [shiftup . shiftleft    , shiftup    , shiftup . shiftright b
              ,shiftleft                           , shiftright b
              ,shiftdown b . shiftleft, shiftdown b, shiftdown b . shiftright b]

applyShifts :: a -> [[a]] -> [[[a]]]
applyShifts b = map transpose . transpose . sequence (allShifts b)

annotateWithNeighbours :: a -> [[a]] -> [[(a,[a])]]
annotateWithNeighbours b l = zipWith zip l (applyShifts b l)

write :: Char -> String -> Char
write '*' = const '*'
write _ = i2d . length . filter (=='*') 
	where
	i2d 0 = ' '
	i2d s = intToDigit s
