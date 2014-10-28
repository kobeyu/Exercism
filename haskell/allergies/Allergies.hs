module Allergies (Allergen(..), isAllergicTo, allergies) where
import Data.Bits

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate | Pollen | Cats
	deriving (Enum, Show,Eq)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a i = testBit i (fromEnum a)

allergies :: Int -> [Allergen]
allergies i = filter (`isAllergicTo` i) [Eggs .. Cats]
