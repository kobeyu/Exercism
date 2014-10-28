module House(rhyme) where
import Data.List(tails)

data Thing = T {act :: String, what :: String} deriving(Show)

things :: [Thing]
things = 
	[ T "" "horse and the hound and the horn"
	, T "belonged to" "farmer sowing his corn"
	, T "kept" "rooster that crowed in the morn"
	, T "woke" "priest all shaven and shorn"
	, T "married" "man all tattered and torn"
	, T "kissed" "maiden all forlorn"
	, T "milked" "cow with the crumpled horn"
	, T "tossed" "dog"
	, T "worried" "cat"
	, T "killed" "rat"
	, T "ate" "malt"
	, T "lay in" "house that Jack built."
	]

verse ::  [Thing] -> String
verse = unlines . zipWith ((unwords . ) . sequence) lineFuncs 
	where
		lineFuncs :: [[Thing -> String]]
		lineFuncs =  [const "This is the",what] 
			: repeat [const "that",act,const "the",what]

rhyme :: String
rhyme = unlines . map verse . reverse . init . tails $ things
