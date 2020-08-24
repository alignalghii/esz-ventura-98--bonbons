{-# LANGUAGE NPlusKPatterns, TypeSynonymInstances #-}

module Board where

import Position (Position, Sizing, allPositions, inTill)
import N (sliceTill, continuousParts)
import Data.Bool (bool)
import Data.Maybe (catMaybes, mapMaybe)
import Option (induce)
import ListX (nonEmptySubsets)

import ShowInd


type BoardProp = Position -> Bool
data Board = Bd BoardProp Sizing

instance Show Board where
    showsPrec _ = shB 0
    showList [] = id
    showList (b : bs) = shows b . (:) '\n' . showList bs


instance ShowInd Board where
    showInd = shB



fullBoard, emptyBoard :: Sizing -> Board
fullBoard  = Bd $ const True
emptyBoard = Bd $ const False

isEmptyBoard :: Board -> Bool
isEmptyBoard (Bd boardProp sizing) = all (not . boardProp) (allPositions sizing)



-- boardStep :: Board -> [Board]
-- baordStep 

{--
boardStep :: Board -> [Board]
boardStep (Bd board) = if all board [(A, A), (A, B), (A, C)]
    then [Bd $ \position -> if position `elem` [(A, A), (A, B), (A, C)] then False else board position]
    else []
--}


boardStep :: Board -> [Board]
boardStep = ruleRows <++> ruleCols <++> ruleCells

ruleRows, ruleCols :: Board -> [Board]
ruleRows board@(Bd _ (_, lng)) = (foldr (<++>) (const []) $ map ruleRow (sliceTill lng)) board
ruleCols board@(Bd _ (_, lng)) = (foldr (<++>) (const []) $ map ruleCol (sliceTill lng)) board

ruleRow, ruleCol :: Int -> Board -> [Board]
ruleRow i board@(Bd boardProp (height, width)) = simpleRules [[(i, j) | j <- part] | part <- continuousParts width ] board
ruleCol j board@(Bd boardProp (height, width)) = simpleRules [[(i, j) | i <- part] | part <- continuousParts height] board

ruleCells :: Board -> [Board]
ruleCells board@(Bd _ sizing) = simpleRules (map (: []) (allPositions sizing)) board

(<++>) :: (a -> [b]) -> (a -> [b]) -> (a -> [b])
f <++> g = \a -> f a ++ g a

boardAct :: Board -> [Position] -> Maybe Board
boardAct board [] = Just board
boardAct board (position : positions) =  boardAct board positions >>= flip boardZeroOut position

boardZeroOut :: Board -> Position -> Maybe Board
boardZeroOut board position = boardSet board position False

simpleRules :: [[Position]] -> Board -> [Board]
simpleRules continuousPositionSets = foldr (<++>) (const []) $ map simpleRule continuousPositionSets

simpleRule :: [Position] -> Board -> [Board]
simpleRule conditions = rule conditions [conditions]  

smartRule :: [Position] -> Board -> [Board]
smartRule conditions = rule conditions (nonEmptySubsets conditions)

rule :: [Position] -> [[Position]] -> Board -> [Board]
rule conditions actionSets board = if and $ mapMaybe (boardGet board) conditions
    then mapMaybe (boardAct board) actionSets
    else []

boardGet :: Board -> Position -> Maybe Bool
boardGet (Bd boardProp sizing) position = if position `inTill` sizing
    then Just $ boardProp position
    else Nothing

boardSet :: Board -> Position -> Bool -> Maybe Board
boardSet board@(Bd boardProp sizing) positionPar flag = if positionPar `inTill` sizing
    then Just $ Bd (\positionArg -> bool (boardProp positionArg) flag (positionArg == positionPar)) sizing
    else Nothing


type Range a = (a, a)

shB :: Int -> Board -> ShowS
shB ind (Bd boardProp (height, lng)) = showSBoard ind boardProp height lng

showBoard :: Int -> BoardProp -> Int -> Int -> String
showBoard ind boardProp height lng = showSBoard ind boardProp height lng ""

showSBoard :: Int -> BoardProp -> Int -> Int -> ShowS
showSBoard ind boardProp height lng = showSBoardRange ind boardProp (0, height) lng

showSBoardRange :: Int -> BoardProp -> Range Int -> Int -> ShowS
showSBoardRange ind  boardProp (row,  0        ) lng = id
showSBoardRange ind boardProp (row, height + 1) lng = showSBoardLine ind boardProp row lng .  showSBoardRange ind boardProp (row + 1, height) lng

showSBoardLine :: Int -> BoardProp -> Int -> Int -> ShowS
showSBoardLine ind boardProp row lng = (replicate ind ' ' ++) . showSBoardLineRange boardProp row (0, lng)

showSBoardLineRange :: BoardProp -> Int -> Range Int -> ShowS
showSBoardLineRange boardProp row (col, 0      ) = ('\n' :)
showSBoardLineRange boardProp row (col, lng + 1) = showSBoardCell boardProp row col . showSBoardLineRange boardProp row (col + 1, lng)

showSBoardCell :: BoardProp -> Int -> Int -> ShowS
showSBoardCell boardProp i j = (:) $ bool '⬜' '⬛' $ boardProp (i, j)
