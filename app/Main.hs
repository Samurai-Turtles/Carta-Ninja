-- Uncomment this section and run "cabal build && cabal run" to check if its working

-- module Main where

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"
module Main where

import ReadData
import Render (action)

main :: IO()
main = do
  -- mudança de estado
  action 