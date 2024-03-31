-- Uncomment this section and run "cabal build && cabal run" to check if its working
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Card
import Gameplay
import State
import CSVManager
import StateManager
import Helpers


-- imports permanentes:: 
import GameLoop
import Ranking
import Render (action)
import Ranking (Ranking(Ranking))


main :: IO ()
main = do
    initLoop
    