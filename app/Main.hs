-- Uncomment this section and run "cabal build && cabal run" to check if its working

-- module Main where

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

import Deck (pushToEnd)

testFn :: [Int]
testFn = pushToEnd 0 [1..10] 

main :: IO ()
main = do
    print testFn
