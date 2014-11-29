import Data.List (unfoldr)

luhn :: Integral a => a -> Bool
luhn num = s `mod` 10 == 0
  where
    s = sum finalDigits
    finalDigits = map (\a -> if a > 9 then a - 9 else a) doubledDigits
    doubledDigits = foldr (\(a, b) c -> (a*b) : c) [] $ zip reverseDigits (cycle [1,2])
    reverseDigits = unfoldr (\a -> if a == 0 then Nothing else Just (a `mod` 10, a `div` 10)) num

