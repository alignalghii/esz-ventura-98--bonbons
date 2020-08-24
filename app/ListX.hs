{-# LANGUAGE TupleSections #-}

module ListX where

descartes :: [a] -> [b] -> [(a, b)]
descartes as bs = as >>= (\a -> map (a,) bs)

descartesSquared :: [a] -> [(a, a)]
descartesSquared as = descartes as as

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (a : as) = subsets as >>= \s -> [s, a : s] -- let ss = subsets as in map (a :) ss ++ ss

nonEmptySubsets :: [a] -> [[a]]
nonEmptySubsets = tail . subsets


indexed :: [a] -> [(Int, a)]
indexed = indexedFrom 0

indexedFrom :: Int -> [a] -> [(Int, a)]
indexedFrom _ [] = []
indexedFrom i (a : as) = (i, a) : indexedFrom (i + 1) as

chainThem :: [a -> a] -> a -> a
chainThem = foldr (.) id
