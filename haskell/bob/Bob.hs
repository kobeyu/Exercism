module Bob(responseFor) where
import Data.Char (isSpace,isAlpha,isLower)

responseFor :: String -> String
responseFor x
	| all isSpace x = "Fine. Be that way!"
	| any isAlpha x && (not . any isLower) x = "Whoa, chill out!"
	| last x == '?' = "Sure."
	| otherwise = "Whatever."