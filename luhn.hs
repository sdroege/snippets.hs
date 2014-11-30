import Data.List (unfoldr)
import Data.Tuple (swap)

luhn :: Integral a => a -> Bool
luhn num = s `mod` 10 == 0
  where
    s = sum finalDigits
    finalDigits = map (\a -> if a > 9 then a - 9 else a) doubledDigits
    doubledDigits = zipWith (*) reverseDigits (cycle [1,2])
    reverseDigits = unfoldr (\a -> if a == 0 then Nothing else Just (swap . (`divMod` 10) $ a)) num

