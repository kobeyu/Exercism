module DNA (hammingDistance) where

hammingDistance :: String -> String -> Int
hammingDistance as = length . filter (uncurry (/=)) . zip as 
