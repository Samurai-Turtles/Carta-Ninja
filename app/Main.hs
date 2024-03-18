-- Uncomment this section and run "cabal build && cabal run" to check if its working

-- module Main where

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"
module Main where

import qualified UI.TemplateData.LeitorData as LD

main :: IO()
main = do
  print (LD.getGameplayData)