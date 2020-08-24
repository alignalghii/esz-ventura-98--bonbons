{-# LANGUAGE NPlusKPatterns #-}

module Test where

f :: Int -> (Int, Int) -> Int -> Int
f a (n, 0) b = n
f a (n, i + 1) b = 1000 * n + i
