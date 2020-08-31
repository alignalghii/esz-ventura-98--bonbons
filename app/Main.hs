{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import ShowInd
import System.Environment (getArgs)
import System.Exit (die)

import System.Console.GetOpt

import Web.Scotty as S hiding (body)
import Data.Text.Lazy (Text, pack)

import Data.Bool (bool)
import Data.Maybe  (isJust)

import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map, param, main)
import Text.Blaze.Html5.Attributes as HA hiding (title, span, form)
import Text.Blaze.Html.Renderer.Pretty
import Control.Monad (forM_)


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
    get "/" routeAction

-- @TODO router with params!

routeAction, homeAction, hackAction :: ActionM ()
routeAction = do
    maybeRowsNum  <- maybeParam "m"
    maybeColsNum  <- maybeParam "n"
    maybeQuestion <- maybeParam "question"
    case (maybeRowsNum, maybeColsNum, maybeQuestion) of
        (Just rowsNum, Just colsNum, Nothing      ) -> holdAction rowsNum colsNum
        (Just rowsNum, Just colsNum, Just question) -> readAction rowsNum colsNum question
        (Nothing     , Nothing     , Nothing)       -> homeAction
        _                                           -> hackAction
homeAction = viewModel Nothing 2 2 undefined
hackAction = S.html $ pack $ renderHtml $ docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        title "Bonbons"
    body $ do
        a ! href "/" $ "Restart entire page from empty state"
        h1 $ preEscapedToHtml ("Bonbons &mdash; hack usage" :: String)
        p "Restart page, hacky usage not allowed"

maybeParam :: Parsable a => Text -> ActionM (Maybe a)
maybeParam name = fmap Just (param name) `rescue` (const $ return Nothing)

holdAction :: Int -> Int -> ActionM ()
holdAction rowsNum colsNum          = viewModel Nothing         rowsNum colsNum undefined

readAction :: Int -> Int -> Text -> ActionM()
readAction rowsNum colsNum question = viewModel (Just question) rowsNum colsNum isSafe where
    isSafe = question == "check" && (rowsNum <= 3 && colsNum <= 4 || rowsNum <= 4 && colsNum <= 3) || question == "trace" && (rowsNum <= 2 && colsNum <= 3 || rowsNum <= 3 && colsNum <= 2)

viewModel :: Maybe Text -> Int -> Int -> Bool -> ActionM ()
viewModel maybeQuestion rowsNum colsNum isSafe = S.html $ pack $ renderHtml $ viewModel_ maybeQuestion rowsNum colsNum isSafe

viewModel_ :: Maybe Text -> Int -> Int -> Bool -> Html
viewModel_ maybeQuestion rowsNum colsNum isSafe = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        title "Bonbons"
    body $ do
        if isJust maybeQuestion
            then a ! href "/" $ "Restart entire page from empty state"
            else span "Restart entire page from empty state"
        h1 "Bonbons"
        h2 "The math problem"
        a ! href "https://qubit.hu/2020/08/17/esz-ventura-jatssz-bonbonnal-es-nyerj-csokit" ! target "_blank" $ "Math problem #98"
        " "
        span "as part of series"
        " "
        a ! href "https://qubit.hu/tag/esz-ventura" ! target "_blank" $ "Ész Ventura"
        h2 "Supplementary infos"
        ul $ do
            li $ do
                "None of the players have tie strategy: exactly one of them has winning strategy, either "
                em "Beginner Player"
                " or "
                em "Follower Player"
                "."
        h2 $ preEscapedToHtml ("The main app form &mdash; You can ask here" :: String)
        form ! action "/" ! method "GET" $ do
            select ! name "m" $ optionIteming04 $ Just rowsNum
            span " rows and "
            select ! name "n" $ optionIteming04 $ Just colsNum
            span " columns "
            button ! type_ "submit" ! name "question" ! value "check" $ "Who has winning strategy?"
            " "
            button ! type_ "submit" ! name "question" ! value "trace" $ "Game tree in raw"
        case maybeQuestion of
            Just question -> do
                h2 "App answer"
                if question == "check"
                    then do
                        h3 "Who has the winning strategy?"
                        p $ do
                            if isSafe
                                then if startPlayerHasWiningStrategy $ game (rowsNum, colsNum)
                                    then do
                                        "It is "
                                        em "Follower Player"
                                        " (Máté) who has the winning strategy"
                                    else do
                                        "It is "
                                        em "Beginner Player"
                                        " (Bogi) who has winning strategy"
                                else "For too long calculation load, this operation is not allowed on a board bigger than 3  x 4 or 4 x 3, here"
                            " on a "
                            toHtml rowsNum
                            " x "
                            toHtml colsNum
                            " board."
                    else do
                        h3 "Tree of the game in raw textual format"
                        if isSafe
                            then textarea ! customAttribute "writeonly" "writeonly" ! cols "30" ! rows "40" $ toHtml $ showInd 0 (game (rowsNum, colsNum)) ""
                            else p $ do
                                "For too long calculation load, this operation is not allowed on a board bigger than 2 x 3 or 3 x 2, here"
                                " on a "
                                toHtml rowsNum
                                " x "
                                toHtml colsNum
                                " board."
            Nothing -> return ()


optionIteming04 :: Maybe Int -> Html
optionIteming04 = optionIteming [0..4] 2

optionIteming :: [Int] -> Int -> Maybe Int -> Html
optionIteming is d msel = forM_ is (\i -> if msel == Just i || msel == Nothing && i == d then option ! value (showv i) ! selected "selected" $ toHtml $ show i else  option ! value (showv i) $ toHtml $ show i)

showv :: Show a => a -> AttributeValue
showv = stringValue . show
