module Queens (boardString, canAttack) where
import Data.Array ((//),elems,listArray,Array)
import Data.List (intersperse)
import Data.List.Split(chunksOf)
import Data.Maybe(fromJust,isJust)
import Control.Arrow(first)

type QueenPos = (Int,Int)

data Cell = O | W | B deriving (Show,Eq)
type Board = Array (Int,Int) Cell

bareBoard :: Board
bareBoard = listArray ((0,0),(7,7)) (repeat O)

printBoard :: Board -> String
printBoard = unlines . map (intersperse ' ' . concatMap show) . chunksOf 8 . elems

boardString :: Maybe QueenPos -> Maybe QueenPos -> String
boardString wq bq = printBoard $ bareBoard // updates
	where updates = map (first fromJust) . filter (isJust . fst) $ [(wq,W),(bq,B)]

canAttack :: QueenPos -> QueenPos -> Bool
canAttack (wqx,wqy) (bqx,bqy) = vert || horiz || diag
	where
		vert = wqx == bqx
		horiz = wqy == bqy
		diag = abs (wqx - bqx) == abs (wqy - bqy)
