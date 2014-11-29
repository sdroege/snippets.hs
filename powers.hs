powers :: [a] -> [[a]]
powers [] = [[]]
powers (x:xs) = pxs `interleave` map (x:) pxs
  where
    pxs = powers xs

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xy [] = xy
interleave (x:xs) (y:ys) = x : y : interleave xs ys

