fib1 :: Num a => Int -> a
fib1 n = fibs !! n

fib2 :: Num a => Int -> a
fib2 0 = 0
fib2 1 = 1
fib2 n
  | n > 0 = fib2 (n-1) + fib2 (n-2)

fib3 :: Num a => Int -> a
fib3 n = fst $ fib3Helper n

fib3Helper :: Num a => Int -> (a, a)
fib3Helper 0 = (0, 0)
fib3Helper 1 = (1, 0)
fib3Helper n
  | n > 0 = (an, an1)
            where
              (an1, an2) = fib3Helper (n-1)
              an = an1 + an2

fibs :: Num a => [a]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]

fibs2 :: Num a => [a]
fibs2 = 0 : 1 : zipWith (+) fibs (tail fibs)
