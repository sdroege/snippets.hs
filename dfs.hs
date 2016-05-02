import qualified Data.Set as S
import Data.Set (Set)

dfs :: (Eq a, Ord a) => (a -> [a]) -> a -> a -> [[a]]
dfs neighboursOf start end = dfs' S.empty start
  where
    dfs' vs cur = if cur == end then
                    -- if we're at the end, return ourselves, aka the only path from cur to end
                    [[cur]]
                  else
                    -- otherwise return all paths from cur to end
                    map (cur :) next
      where
        ns = filter (not . flip S.member vs) (neighboursOf cur)
        vs' = S.insert cur vs
        -- All paths from cur to end excluding cur
        next = concatMap (dfs' vs') ns

