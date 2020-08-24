module PositionN where

import N
import ListX

type Position = (N, N)

allPositions :: [Position]
allPositions = descartesSquared rangeN
