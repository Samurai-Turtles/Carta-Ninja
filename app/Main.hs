-- Uncomment this section and run "cabal build && cabal run" to check if its working
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Card
import Gameplay
import State
import CSVManager
import StateManager
import Helpers
import Ranking

-- testGetWinner :: IO ()
-- testGetWinner = do
--     let card1 = Card 1 "fire" 10
--     let card2 = Card 1 "nature" 10
--     print "=== getWinner ==="
--     print $ getWinner card1 card2 -- Expect:  1
--     print $ getWinner card2 card1 -- Expect: -1
--     print $ getWinner card1 card1 -- Expect:  0

-- testLevelUp :: IO ()
-- testLevelUp = do
--     print "=== levelUp ==="
--     print levelUpPlayer -- Expect: 1

-- testUpdatePlayerLife :: IO ()
-- testUpdatePlayerLife = do
--     print "=== updatePlayerLife ==="
--     print $ updatePlayerLife 1    -- Expect: 1
--     print $ updatePlayerLife (-1) -- Expect: -1

-- testUpdatePlayerCP :: IO ()
-- testUpdatePlayerCP = do
--     print "=== updatePlayerCP ==="
--     print $ updatePlayerCP 200    -- Expect: 200
--     print $ updatePlayerCP (-100) -- Expect: -100

main :: IO ()
main = do
    -- let c1 = Card 01 "fire" 1
    -- let c2 = Card 02 "water" 2
    -- let list = [c1, c2]
    -- print list

    -- let list2 = getRanking
    -- print list2

    -- let list3 = returnLiterallyTheSameList getCards
    -- print list3

    -- print getRanking

    -- saveRankingCSV "Fulano" 999

    -- print getRanking

    -- saveRankingCSV "Fulano" 333

    -- print getRanking

    -- x <- getBattleState
    -- print x


    -- writeGlobalState $ GlobalState {screen = "abaco"}
    -- aaa <- getGlobalState
    -- print $ screen aaa

    -- getLine

    -- writeGlobalState $ GlobalState {screen = "babaco"}
    -- bbb <- getGlobalState
    -- print $ screen bbb

    -- getLine

    -- writeGlobalState $ GlobalState {screen = "cabaco"}
    -- ccc <- getGlobalState
    -- print $ screen ccc

    print $ organizeRankings [
        Ranking {name="Ruivas", points=30},
        Ranking {name="Loiras", points=3423423423423423434},
        Ranking {name="Ruivas", points=234},
        Ranking {name="Ruivas", points=32532},
        Ranking {name="Loiras", points=3423}
        ]


    -- testGetWinner
    -- testLevelUp
    -- testUpdatePlayerLife
    -- testUpdatePlayerCP
