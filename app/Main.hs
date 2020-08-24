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

data Flag = Help | Web | Desktop | RowsNum String | ColsNum String

optDescriptions :: [OptDescr Flag]
optDescriptions =
    [    Option "h?" ["help", "info"]  (NoArg Help)               "Help for usage"
    ,    Option "wS" ["web", "server"] (NoArg Web)                "Web application mode (server)"
    ,    Option "d"  ["desktop"]       (NoArg Desktop)            "Desktop application mode (simple command-line)"
    ,    Option "mr" ["rowsnum"]       (ReqArg RowsNum "ROWSNUM") "ROWSNUM stands for a natural number (0, 1, 2, ...) for the number of rows"
    ,    Option "nc" ["colsnum"]       (ReqArg ColsNum "COLSNUM") "COLSNUM stands for a natural number (0, 1, 2, ...) for the number of columns"
    ]

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute optDescriptions args of
        ([Desktop, RowsNum m, ColsNum n], nonOpts, errorMsgs) -> desktopMain m n
        ([Web]                          , nonOpts, errorMsgs) -> webMain
        ([Help]                         , nonOpts, errorMsgs) -> putStrLn info
        (_                              , nonOpts, errorMsgs) -> die      info

info :: String
info = usageInfo "Usage:\n\tRunning as dektop application:.....    bonbons --desktop -m ROWSNUM -n COLSNUM\n\tRunning as web application:........    bonbons --server\nOptions:" optDescriptions

desktopMain :: String -> String -> IO ()
desktopMain rowsNumString colsNumString = case map reads [rowsNumString, colsNumString] of
    [(rowsNum, _) : _, (colsNum, _): _] -> if rowsNum >= 0 && rowsNum >= 0 then putStrLn $ showInd 0 (game (rowsNum, colsNum)) "" else failValidation
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
