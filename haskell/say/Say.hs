module Say(inEnglish) where
import Data.List(unfoldr)
import Data.Tuple(swap)
import Data.Map (Map, (!), fromAscList)
import Data.Functor ((<$>))

units :: (Integral a) => Map a String
units = fromAscList . zip [1..19] . words $
    "one two three four five six seven eight nine ten "++
    "eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"

tens :: (Integral a) => Map a String
tens = fromAscList . zip [2..9] $ words "twenty thirty forty fifty sixty seventy eighty ninety"

names :: [String]
names = "" : words "thousand million billion"

repMod :: (Integral a) => a -> a -> [a]
repMod b = unfoldr step
    where
    step 0 = Nothing
    step a = Just . swap . (`divMod` b) $ a

inEnglish :: (Integral a, Num a) => a -> Maybe String
inEnglish 0 = Just "zero"
inEnglish x | x < 0|| fromIntegral x >= ((10^12)::Integer) = Nothing
inEnglish x = do
    res <- (fmap concat . sequence . reverse . zipWith f names . map upto999 . repMod 1000) x
    return . unwords $ res
  where
    f :: String -> Maybe[String] -> Maybe [String]
    f "" n = n
    f _ (Just []) = Just []
    f groupName n = fmap (++[groupName]) n

upto999 :: (Integral a, Num a) => a -> Maybe [String]
upto999 x
    | x == 0 = Just []
    | x < 20 = Just . (:[]) $ units ! x
    | x <= 99 && head ds == 0 = Just . (:[]) $ tens ! (ds!!1)
    | x <= 99 = Just . (:[]) $ tens ! (ds!!1) ++ "-" ++ units ! head ds
    | x <= 999 = ([units ! (ds!!2), "hundred"]++) <$> upto999 (x `mod` 100)
    | otherwise = Nothing
    where
      ds = repMod 10 x
