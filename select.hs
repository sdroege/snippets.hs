-- From https://www.reddit.com/r/haskell/comments/33qjyw/unique_sampling_drawing_searches_with_list_and/

zippers :: [a] -> [([a], a, [a])]
zippers = go []
    where
        go _ [] = []
        go b (x:xs) = (b, x, xs) : go (x:b) xs

select, select' :: [a] -> [(a, [a])]
select  xs = [(v, reverse b ++ e) | (b, v, e) <- zippers xs]
select' xs = [(v,         b ++ e) | (b, v, e) <- zippers xs]

