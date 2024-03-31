{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Card
import State
import Helpers
import Gameplay
import CSVManager
import StateManager
import GameLoop


main :: IO ()
main = do
    initLoop
    