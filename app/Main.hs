-- Uncomment this section and run "cabal build && cabal run" to check if its working

module Main where

import Card
import Gameplay

testGetWinner :: IO ()
testGetWinner = do
    let card1 = Card 1 "Fogo" 10
    let card2 = Card 1 "Natureza" 10
    print "=== getWinner ==="
    print $ getWinner card1 card2 -- Expect:  1
    print $ getWinner card2 card1 -- Expect: -1
    print $ getWinner card1 card1 -- Expect:  0

testLevelUp :: IO ()
testLevelUp = do
    print "=== levelUp ==="
    print levelUpPlayer -- Expect: 2

testUpdatePlayerLife :: IO ()
testUpdatePlayerLife = do
    print "=== updatePlayerLife ==="
    print $ updatePlayerLife 1    -- Expect: 2
    print $ updatePlayerLife (-1) -- Expect: 0

testUpdatePlayerCP :: IO ()
testUpdatePlayerCP = do
    print "=== updatePlayerCP ==="
    print $ updatePlayerCP 200    -- Expect: 300
    print $ updatePlayerCP (-100) -- Expect: 0

main :: IO ()
main = do
    testGetWinner
    testLevelUp
    testUpdatePlayerLife
    testUpdatePlayerCP
