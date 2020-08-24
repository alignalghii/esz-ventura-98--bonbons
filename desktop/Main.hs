module Main where

import Game
import ShowInd
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case map reads args of
        [(height, _) : _, (width, _): _] -> putStrLn $ showInd 0 (game (height, width)) ""
        _                           -> die "Usage: <programname> <boardheight> <boardwidth>"
