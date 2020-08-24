{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import ShowInd
import System.Environment (getArgs)
import System.Exit (die)

import System.Console.GetOpt

import Web.Scotty
import Data.Monoid (mconcat)
import Web.Scotty.Internal.Types (ActionT)
import Data.Text.Internal.Lazy (Text)

import Data.Bool (bool)


data Flag = Help | Server | ConsoleTrace | ConsoleCheck | RowsNum String | ColsNum String

optDescriptions :: [OptDescr Flag]
optDescriptions =
    [    Option "h?" ["help", "info"]  (NoArg Help)               "Help for usage"
    ,    Option "wS" ["web", "server"] (NoArg Server)             "Web application mode (server)"
    ,    Option "t"  ["console-trace"] (NoArg ConsoleTrace)       "Console utility to list game tree as rawtext"
    ,    Option "c"  ["console-check"] (NoArg ConsoleCheck)       "Console utility to check solubility (`Has Begginer Player a winning strategy or not?')"
    ,    Option "m"  ["rowsnum"]       (ReqArg RowsNum "ROWSNUM") "ROWSNUM stands for a natural number (0, 1, 2, ...) for the number of rows"
    ,    Option "n"  ["colsnum"]       (ReqArg ColsNum "COLSNUM") "COLSNUM stands for a natural number (0, 1, 2, ...) for the number of columns"
    ]

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute optDescriptions args of
        ([Server]                            , nonOpts, errorMsgs) -> webMain
        ([ConsoleTrace, RowsNum m, ColsNum n], nonOpts, errorMsgs) -> traceMain m n
        ([ConsoleCheck, RowsNum m, ColsNum n], nonOpts, errorMsgs) -> checkMain m n
        ([Help]                              , nonOpts, errorMsgs) -> putStrLn info
        (_                                   , nonOpts, errorMsgs) -> die      info

info :: String
info = usageInfo "Usage:\n\tRunning as dektop application:.....    bonbons --desktop -m ROWSNUM -n COLSNUM\n\tRunning as web application:........    bonbons --server\nOptions:" optDescriptions

traceMain :: String -> String -> IO ()
traceMain rowsNumString colsNumString = case map reads [rowsNumString, colsNumString] of
    [(rowsNum, _) : _, (colsNum, _): _] -> if rowsNum >= 0 && rowsNum >= 0 then putStrLn $ showInd 0 (game (rowsNum, colsNum)) "" else failValidation
    _                                -> failValidation

checkMain :: String -> String -> IO ()
checkMain rowsNumString colsNumString = case map reads [rowsNumString, colsNumString] of
    [(rowsNum, _) : _, (colsNum, _): _] -> if rowsNum >= 0 && rowsNum >= 0 then putStrLn $ bool "Follower Player has winning strategy" "Beginner Player has winning strategy" $ startPlayerHasWiningStrategy $ game (rowsNum, colsNum) else failValidation
    _                                -> failValidation

failValidation :: IO ()
failValidation = die "bonbons --desktop -m ROWSNUM -n COLSNUM      : ROWSNUM and COLSNUM are nummeric arguments that must be natural numbers!"

webMain :: IO ()
webMain = scotty 3000 router

router :: ScottyM ()
router = do
    get "/:word" beamAction

beamAction :: Web.Scotty.Internal.Types.ActionT Data.Text.Internal.Lazy.Text IO ()
beamAction = do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!"]
