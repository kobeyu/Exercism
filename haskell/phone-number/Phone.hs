module Phone (areaCode, number, prettyPrint) where
import Data.Char(isDigit)

--this function is here to allow our phone parsing to work for infinite strings...
lengthIs :: Int -> [a] -> Bool
lengthIs t = (==t) . length . take (t+1) 

number :: String -> String
number = purenumber . filter isDigit
	where
		purenumber n
			| lengthIs 10 n = n
			| lengthIs 11 n && head n == '1' = tail n
			| otherwise = "0000000000"

prettyPrint:: String -> String
prettyPrint = p . number 
	where 
		p [a,b,c,d,e,f,g,h,i,j] = ['(',a,b,c,')',' ',d,e,f,'-',g,h,i,j]
		p _ = error "Not 10 digits"

areaCode :: String->String
areaCode = take 3 . number
