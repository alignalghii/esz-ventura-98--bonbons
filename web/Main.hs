{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Monoid (mconcat)

main :: IO ()
main = scotty 3000 router


router = do
    get "/:world" worldAction

worldAction = do
    beam <- param "world"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
