import qualified Data.Set as S
import Data.Set (Set)
import Data.Traversable (mapAccumL)
import Data.Maybe (catMaybes)

nubOrd :: Ord a => [a] -> [a]
nubOrd = catMaybes . snd . mapAccumL (\s a -> if a `S.member` s then (s, Nothing) else (S.insert a s, Just a)) S.empty

