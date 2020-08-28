{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import ShowInd
import System.Environment (getArgs)
import System.Exit (die)

import System.Console.GetOpt

import Web.Scotty
import Data.Monoid ((<>), mconcat)
import Web.Scotty.Internal.Types (ActionT)
import Data.Text.Lazy (Text, pack)

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
info = usageInfo "Usage:\n\tRunning as dektop application:.....    bonbons --console-check -m ROWSNUM -n COLSNUM ..... Has Beginner Player the winning strategy?\n                                               bonbons --console-trace -m ROWSNUM -n COLSNUM ..... Print the entire game tree!\n\tRunning as web application:........    bonbons --server\nOptions:" optDescriptions

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
    get "/" homeAction

homeAction, homeAction1, homeAction2 :: ActionM ()
homeAction = homeAction1 `rescue` (const homeAction2)
homeAction1 = do
    question <- param "question" :: ActionM Text
    rowsNum <- param "m" :: ActionM Int
    colsNum <- param "n" :: ActionM Int
    if question == "check" && (rowsNum <= 3 && colsNum <= 4 || rowsNum <= 4 && colsNum <= 3) || question == "trace" && (rowsNum <= 2 && colsNum <= 3 || rowsNum <= 3 && colsNum <= 2) 
        then html $ mconcat $ ["<!DOCTYPE html>\n<html>\n\t<head>\n\t\t<meta charset=\"UTF-8\"/>\n\t\t<title>Bonbons</title>\n\t</head>\n\t<body>\n\t\t<a href=\"/\">Restart entire page from empty state</a>\n\t\t<h1>Bonbons</h1>\n\t\t<h2>The math problem</h2>\n\t\t<a href=\"https://qubit.hu/2020/08/17/esz-ventura-jatssz-bonbonnal-es-nyerj-csokit\" target=\"_blank\">Math problem #98</a>\n\t\t<span>as part of series</span>\n\t\t<a href=\"https://qubit.hu/tag/esz-ventura\" target=\"_blank\">Ész Ventura</a>.\n\t\t<h2>Supplementary infos</h2>\n\t\t<ul>\n\t\t\t<li>None of the players have tie strategy: exactly one of them has winning strategy, either <em>Beginner Player</em> or <em>Follower Player</em>.</li>\n\t\t</ul>\n\t\t<h2>The main app form &mdash; You can ask here</h2>\n\t\t<form action=\"/\" method=\"GET\">\n\t\t\t<select name=\"m\">", optionIteming04 $ Just rowsNum, "\n\t\t\t</select>\n\t\t\t<span>rows and</span>\n\t\t\t<select name=\"n\">", optionIteming04 $ Just colsNum, "\n\t\t\t</select>\n\t\t\t<span>columns</span>\n\t\t\t<button type=\"submit\" name=\"question\" value=\"check\">Who has winning strategy?</button>\n\t\t\t<button type=\"submit\" name=\"question\" value=\"trace\">Game tree in raw</button>\n\t\t</form>\n\t\t<h2>App answer</h2>"] ++ (if question == "check" then ["<h3>Who has the winning strategy?</h3>", bool "\n\t\t<p>It is <em>Follower Player</em> (Máté) who has the winning strategy" "\n\t\t<p>It is <em>Beginner Player</em> (Bogi) who has winning strategy" $ startPlayerHasWiningStrategy $ game (rowsNum, colsNum), " on a ", showt rowsNum, " X ", showt colsNum, " board.</p>"] else ["\n\t\t<h3>Tree of the game in raw textual format</h3>", "\n\t\t<textarea writeonly cols=\"30\" rows=\"40\">\n", pack $ showInd 0 (game (rowsNum, colsNum)) "", "\t\t</textarea>"]) ++ ["\n\t</body>\n</html>"]
        else html $ mconcat $ ["<!DOCTYPE html>\n<html>\n\t<head>\n\t\t<meta charset=\"UTF-8\"/>\n\t\t<title>Bonbons</title>\n\t</head>\n\t<body>\n\t\t<a href=\"/\">Restart entire page from empty state</a>\n\t\t<h1>Bonbons</h1>\n\t\t<h2>The math problem</h2>\n\t\t<a href=\"https://qubit.hu/2020/08/17/esz-ventura-jatssz-bonbonnal-es-nyerj-csokit\" target=\"_blank\">Math problem #98</a>\n\t\t<span>as part of series</span>\n\t\t<a href=\"https://qubit.hu/tag/esz-ventura\" target=\"_blank\">Ész Ventura</a>\n\t\t<h2>Supplementary infos</h2>\n\t\t<ul>\n\t\t\t<li>None of the players have tie strategy: exactly one of them has winning strategy, either <em>Beginner Player</em> or <em>Follower Player</em>.</li>\n\t\t</ul>\n\t\t<h2>The main app form &mdash; You can ask here</h2>\n\t\t<form action=\"/\" method=\"GET\">\n\t\t\t<select name=\"m\">", optionIteming04 $ Just rowsNum, "\n\t\t\t</select>\n\t\t\t<span>rows and</span>\n\t\t\t<select name=\"n\">", optionIteming04 $ Just colsNum, "\n\t\t\t</select>\n\t\t\t<span>columns</span>\n\t\t\t<button type=\"submit\" name=\"question\" value=\"check\">Who has winning strategy?</button>\n\t\t\t<button type=\"submit\" name=\"question\" value=\"trace\">Game tree in raw</button>\n\t\t</form>\n\t\t<h2>App answer</h2>"] ++ (if question == "check" then ["\n\t\t<h3>Who has the winning strategy?</h3><p>For too long calculation load, this operation is not allowed on a board bigger than 3  x 4 or 4 x 3, here on a ", showt rowsNum, " x ", showt colsNum, " board.</p>"] else ["\n\t\t<h3>Tree of the game in raw textual format</h3><p>For too long calculation load, this operation is not allowed on a board bigger than 2 x 3 or 3 x 2, here on a ", showt rowsNum, " x ", showt colsNum, " board.</p>"]) ++ ["\n\t</body>\n</html>"]
homeAction2 = html $ mconcat ["<!DOCTYPE html>\n<html>\n\t<head>\n\t\t<meta charset=\"UTF-8\"/>\n\t\t<title>Bonbons</title>\n\t</head>\n\t<body>\n\t\t<span>Restart entire page from empty state</span>\n\t\t<h1>Bonbons</h1>\n\t\t<h2>The math problem</h2>\n\t\t<a href=\"https://qubit.hu/2020/08/17/esz-ventura-jatssz-bonbonnal-es-nyerj-csokit\" target=\"_blank\">Math problem #98</a>\n\t\t<span>as part of series</span>\n\t\t<a href=\"https://qubit.hu/tag/esz-ventura\" target=\"_blank\">Ész Ventura</a>\n\t\t<h2>Supplementary infos</h2>\n\t\t<ul>\n\t\t\t<li>None of the players have tie strategy: exactly one of them has winning strategy, either the Beginner or the Follower player.</li>\n\t\t</ul>\n\t\t<h2>The main app form &mdash; You can ask here</h2>\n\t\t<form action=\"/\" method=\"GET\">\n\t\t\t<select name=\"m\">", optionIteming04 Nothing, "\n\t\t\t</select>\n\t\t\t<span>rows and</span>\n\t\t\t<select name=\"n\">", optionIteming04 Nothing, "\n\t\t\t</select>\n\t\t\t<span>columns</span>\n\t\t\t<button type=\"submit\" name=\"question\" value=\"check\">Who has winning strategy?</button>\n\t\t\t<button type=\"submit\" name=\"question\" value=\"trace\">Game tree in raw</button>\n\t\t</form>\n\t</body>\n</html>"]

optionIteming04 :: Maybe Int -> Text
optionIteming04 = optionIteming [0..4] 2

optionIteming :: [Int] -> Int -> Maybe Int -> Text
optionIteming is d msel = mconcat ["\n\t\t\t\t<option value=\"" <> showt i <> "\"" <> bool "" " selected" (msel == Just i || msel == Nothing && i == d) <> ">" <> showt i <>  "</option>" | i <- is]

showt :: Show a => a -> Text
showt = pack . show
