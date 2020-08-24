module Game where

import LabelledTree
import Board
import Position (Sizing)
import Data.Bool (bool)
import ShowInd

data Step = St Bool Board

instance Show Step where
    showsPrec _ = showSStep 0

instance ShowInd Step where
    showInd = showSStep

origStep :: Sizing -> Step
origStep sizing = St True $ fullBoard sizing

type Game = LabelledTree Step

gameFrom :: Step -> LabelledTree Step
gameFrom step@(St who board) = HenceThese step $ map gameFrom steps where
    steps = map (St $ not who) (boardStep board)

game :: Sizing -> LabelledTree Step
game = gameFrom . origStep


showSStep :: Int -> Step -> ShowS
showSStep n (St flag board) = shB n board . (++) (replicate n ' ' ++ bool "Máté" "Bogi" flag ++ "\n")


startPlayerHasWiningStrategy :: Game -> Bool
startPlayerHasWiningStrategy (HenceThese (St startPlayer _) opponentGames) = any (not . startPlayerHasWiningStrategy) opponentGames
