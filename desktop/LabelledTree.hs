module LabelledTree where

import ListX (indexed, chainThem)
import ShowInd
import Data.Bool

data LabelledTree a = HenceThese a [LabelledTree a]

depth :: LabelledTree a -> Int
depth (HenceThese a []            ) = 0
depth (HenceThese a (tree : trees)) = 1 + maximum (map depth (tree : trees))

instance Show a => Show (LabelledTree a) where
    showsPrec _ = showSTree []
    showList [] = (++) "\n"
    showList (tree : trees) = shows tree . showList trees

instance ShowInd a => ShowInd (LabelledTree a) where
    showInd n (HenceThese a subtrees) = showInd n a . openBelow n subtrees

openBelow :: ShowInd a => Int -> [LabelledTree a] -> ShowS
openBelow n subtrees = chainThem (map (showInd $ succ n) subtrees) . closeIfEmpty n subtrees

closeIfEmpty :: Int -> [a] -> ShowS
closeIfEmpty n = bool id ((replicate n ' ' ++ "---END---\n") ++) . null

showSTree :: Show a => [Int] -> LabelledTree a -> ShowS
showSTree labels (HenceThese a subtrees) = (++) "\n====" . (shows $ reverse labels) . (++) "=====\n" . shows a . foldr (.) id (map (\(i, tree) -> showSTree (i : labels) tree) (indexed subtrees)) 
