{-# LANGUAGE BangPatterns #-}

iterate' f !x = x : iterate' f (f x)

count b = map reverse (iterate' go [])
    where
        go []        = [1]
        go (x:(!xs)) = if x' == b then
                          0  : xs'
                       else
                          x' : xs
            where
                x'  = x + 1
                xs' = go xs

main = print $ count 2 !! 100000000
