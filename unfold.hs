{-# LANGUAGE BangPatterns #-}
import Data.Monoid

import Control.Applicative
import Data.Functor.Identity

import Data.Bifunctor

import Criterion.Main

-- Standard list based implementation
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f = go
    where
        go b = maybe [] step (f b)
            where
                step (!a, !b') = a : go b'

-- Strict in accumulator/result
unfoldl :: (b -> Maybe (a, b)) -> b -> [a]
unfoldl f = go []
    where
        go accum b = maybe accum step (f b)
            where
                step (!a, !b') = let accum' = a : accum in
                                accum' `seq` go accum' b'

-- Implementation with fold function
unfoldrWith :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
unfoldrWith f c0 g = go
    where
        go b = maybe c0 step (g b)
            where
                step (!a, !b') = a `f` go b'


unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' = unfoldrWith (:) []

-- Strict in accumulator/result
unfoldlWith :: (c -> a -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
unfoldlWith f c0 g = go c0
    where
        go accum b = maybe accum step (g b)
            where
                step (!a, !b') = let accum' = accum `f` a in
                                accum' `seq` go accum' b'

unfoldl' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldl' = unfoldlWith (flip (:)) []

-- Implementation with Monoid
unfoldrM :: Monoid m => (b -> Maybe (m, b)) -> b -> m
unfoldrM f = go
    where
        go b = maybe mempty step (f b)
            where
                step (!m, !b') = m <> go b'

unfoldrMM :: (Functor m, Monad m, Monoid t) => (b -> m (Maybe (t, b))) -> b -> m t
unfoldrMM f = go
    where
        go b = f b >>= maybe (return mempty) step
            where
                step (!t, !b') = (t <>) <$> go b'

unfoldr'' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr'' f = unfoldrM (fmap (first (: [])) . f)

unfoldrWith' :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
unfoldrWith' f c0 g b0 = appEndo (unfoldrM f' b0) c0
    where
        f' = fmap (first (Endo . f)) . g

-- Strict in accumulator/result
unfoldlM :: Monoid m => (b -> Maybe (m, b)) -> b -> m
unfoldlM f = go mempty
    where
        go accum b = maybe accum step (f b)
            where
                step (!m, !b') = let accum' = m <> accum in
                                accum' `seq` go accum' b'

unfoldlMM :: (Functor m, Monad m, Monoid t) => (b -> m (Maybe (t, b))) -> b -> m t
unfoldlMM f = go mempty
    where
        go accum b = f b >>= maybe (return accum) step
            where
                step (!m, !b') = let accum' = m <> accum in
                                accum' `seq` go accum' b'

unfoldl'' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldl'' f = unfoldlM (fmap (first (: [])) . f)

unfoldlWith' :: (c -> a -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
unfoldlWith' f c0 g b0 = appEndo (unfoldlM f' b0) c0
    where
        f' = fmap (first (Endo . flip f)) . g

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks = [  bgroup "unfoldr" [ bench "unfoldr" $ whnf (sum . unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 , bench "unfoldrWith" $ whnf (unfoldrWith (+) 0 (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 , bench "unfoldr'" $ whnf (sum . unfoldr' (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 , bench "unfoldrM" $ whnf (getSum . unfoldrM (\b -> if b == 0 then Nothing else Just (Sum (b `mod` 10), b `div` 10))) (1234^1234)
                                 , bench "unfoldrMM" $ whnf (getSum . runIdentity . unfoldrMM (\b -> return $ if b == 0 then Nothing else Just (Sum (b `mod` 10), b `div` 10))) (1234^1234)
                                 , bench "unfoldrWith'" $ whnf (unfoldrWith' (+) 0 (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 , bench "unfoldr''" $ whnf (sum . unfoldr'' (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 ]
             ,  bgroup "unfoldl" [ bench "unfoldl" $ whnf (sum . unfoldl (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 , bench "unfoldlWith" $ whnf (unfoldlWith (+) 0 (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 , bench "unfoldl'" $ whnf (sum . unfoldl' (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 , bench "unfoldlM" $ whnf (getSum . unfoldlM (\b -> if b == 0 then Nothing else Just (Sum (b `mod` 10), b `div` 10))) (1234^1234)
                                 , bench "unfoldlMM" $ whnf (getSum . runIdentity . unfoldlMM (\b -> return $ if b == 0 then Nothing else Just (Sum (b `mod` 10), b `div` 10))) (1234^1234)
                                 , bench "unfoldlWith'" $ whnf (unfoldlWith' (+) 0 (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 , bench "unfoldl''" $ whnf (sum . unfoldl'' (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10))) (1234^1234)
                                 ]
             ]
