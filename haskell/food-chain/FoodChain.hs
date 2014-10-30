module FoodChain {-(song)-} where
import Data.Maybe(catMaybes)
import Data.List(tails)
import Text.Printf(printf)

song :: String
song = unlines . map (unlines . verse) . tail . reverse . tails $ animals

data Animal = A {lethal :: Bool
                ,targetname :: String 
                ,desc :: Maybe String}
                
name :: Animal -> String
name = takeWhile (/=' ') . targetname

animals :: [Animal]
animals = 
	[A True "horse" $ Just "She's dead, of course!"
	,A False "cow" $ Just "I don't know how she swallowed a cow!"
	,A False "goat" $ Just "Just opened her throat and swallowed a goat!"
	,A False "dog" $ Just "What a hog, to swallow a dog!"
	,A False "cat" $ Just "Imagine that, to swallow a cat!"
	,A False "bird" $ Just "How absurd to swallow a bird!"
	,(\x -> A False ("spider that" ++ x) (Just $ "It"++x++".")) " wriggled and jiggled and tickled inside her" 
	,A False "fly" Nothing 
	]

verse :: [Animal] -> [String]
verse [] = []
verse (a:as) = catMaybes (Just first : desc a : rest)
   where 
   		first = printf "I know an old lady who swallowed a %s." (name a)
   		rest = if lethal a then [] else middle ++ [Just final]
		middle = map Just $ zipWith verseCont (a:as) as
   		final = "I don't know why she swallowed the fly. Perhaps she'll die."

verseCont :: Animal -> Animal -> String
verseCont p a = printf "She swallowed the %s to catch the %s." (name p) (targetname a)
