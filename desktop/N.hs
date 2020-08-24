{-# LANGUAGE NPlusKPatterns #-}

module N where

sliceTill :: Int -> [Int]
sliceTill 0 = []
sliceTill (n + 1) = [0 .. n]

continuousParts :: Int -> [[Int]]
continuousParts n = [[x .. x+lng-1] | lng <- reverse [2 .. n], x <- [0 .. n - lng]]
