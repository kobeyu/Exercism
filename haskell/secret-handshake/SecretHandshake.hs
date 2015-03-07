module SecretHandshake(handshake) where
import Control.Conditional
import Control.Arrow
import Data.Tuple(swap)
import Data.List(foldl',unfoldr)

class BinaryLike a where
    toBinary :: a -> Maybe [Bool]

class BinaryElement a where
    listToBinary :: [a] -> Maybe [Bool]

instance BinaryElement a => BinaryLike [a] where
    toBinary = listToBinary

integralToBinary :: (Integral a) => a -> Maybe [Bool]
integralToBinary = Just . map (==1) . repMod 2

instance BinaryLike Int where
    toBinary = integralToBinary
instance BinaryLike Integer where
    toBinary = integralToBinary

instance BinaryElement Char where
    listToBinary = sequence . reverse . map f
        where
            f '0' = Just False
            f '1' = Just True
            f _   = Nothing

repMod :: (Integral a) => a -> a -> [a]
repMod b = unfoldr step
    where
    step 0 = Nothing
    step a = Just . swap . (`divMod` b) $ a

actions :: [(Bool,[String]) -> (Bool,[String])]
actions = map (second . (:)) ["wink","double blink","close your eyes","jump"] ++ [first not]

handshake :: BinaryLike a => a -> [String]
handshake = maybe [] f . toBinary
    where
        f :: [Bool] -> [String]
        f = uncurry (bool reverse id) . foldl' (flip ($)) (False,[]) . zipWith (??id) actions
