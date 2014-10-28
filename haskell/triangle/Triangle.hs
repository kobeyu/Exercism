module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral | Isosceles | Scalene | Illogical deriving (Eq,Show)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c
	| not $ isTri a b c = Illogical
	| a == b && b == c = Equilateral
	| a == b || b == c || a == c = Isosceles
	| otherwise = Scalene

isTri :: Int -> Int -> Int -> Bool
isTri a b c = (a + b > c) && (a + c > b) && (b + c > a)
