{-# LANGUAGE OverloadedStrings #-}
module WordProblem (answer) where
import Data.Attoparsec.Text(feed,maybeResult,parse,Parser,choice,signed,decimal,many1,skipSpace)
import Control.Applicative((*>))
import Data.Text(pack)
import Data.List(foldl')

answer :: String -> Maybe Int
answer = maybeResult . (`feed` pack "") . parse qParse . pack

parseOp :: Parser (Int -> Int -> Int)
parseOp = choice
    ["plus" *> return (+)
    ,"minus" *> return (-)
    ,"multiplied by" *> return (*)
    ,"divided by" *> return div
    ] 

opInt :: Parser (Int -> Int)
opInt = do
	op <- skipSpace *> parseOp
	i <- skipSpace *> signed decimal
	return (`op` i)

qParse :: Parser Int
qParse = do
	i1 <- "What is" *> skipSpace *> signed decimal
	opi <- many1 opInt
	"?" *> return (foldl' (flip ($)) i1 opi)
