-- Uncomment this section and run "cabal build && cabal run" to check if its working

module Main where

import Card
import Gameplay

testGetWinner :: IO ()
testGetWinner = do
    let card1 = Card 1 "fire" 10
    let card2 = Card 1 "nature" 10
    print "=== getWinner ==="
    print $ getWinner card1 card2 -- Expect:  1
    print $ getWinner card2 card1 -- Expect: -1
    print $ getWinner card1 card1 -- Expect:  0

testLevelUp :: IO ()
testLevelUp = do
    print "=== levelUp ==="
    print levelUpPlayer -- Expect: 1

testUpdatePlayerLife :: IO ()
testUpdatePlayerLife = do
    print "=== updatePlayerLife ==="
    print $ updatePlayerLife 1    -- Expect: 1
    print $ updatePlayerLife (-1) -- Expect: -1

testUpdatePlayerCP :: IO ()
testUpdatePlayerCP = do
    print "=== updatePlayerCP ==="
    print $ updatePlayerCP 200    -- Expect: 200
    print $ updatePlayerCP (-100) -- Expect: -100

main :: IO ()
main = do
    testGetWinner
    testLevelUp
    testUpdatePlayerLife
    testUpdatePlayerCP
