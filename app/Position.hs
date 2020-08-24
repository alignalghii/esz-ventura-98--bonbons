module Position where

import N (sliceTill)
import ListX (descartes)

type Position = (Int, Int)
type Sizing   = (Int, Int)

allPositions :: Sizing -> [Position]
allPositions (m, n) = descartes (sliceTill m) (sliceTill n)


inTill :: Position -> Sizing -> Bool
(i, j) `inTill` (m, n) = all (>= 0) [i, j, m, n] && i < m && j < n
