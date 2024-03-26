{-
    Módulo referente ao carregamento das cartas via CSV
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module CSVManager where

import Card
import Ranking
import Text.CSV
import System.IO.Unsafe (unsafePerformIO)
import System.IO

paths :: [FilePath]
paths = ["app/data/csv/cards.csv",
         "app/data/csv/ranking.csv"]

csvParseError :: p -> [a]
csvParseError csvFile = []

csvParseDone :: [a] -> [a]
csvParseDone = tail

getCSV :: Int -> [Record]
getCSV i = do
    let file = unsafePerformIO (readFile $ paths !! i)
    let csvFile = parseCSV (paths !! i) file
    either csvParseError csvParseDone csvFile

toInt :: String -> Int
toInt n = read n :: Int

recordToCards :: [Record] -> [Card]
recordToCards [] = []
recordToCards (x:xs) = do
    let c = Card (toInt $ head x) (x!!1) (toInt $ x!!2)
    c : recordToCards xs

recordToRanking :: [Record] -> [Ranking]
recordToRanking [] = []
recordToRanking (x:xs) = do
    let r = Ranking (head x) (toInt $ x !! 1)
    r : recordToRanking xs

getCards :: [Card]
getCards = recordToCards (getCSV 0)

getRanking :: [Ranking]
getRanking = recordToRanking (getCSV 1)

cardToCSV :: Int -> String -> Int -> String
cardToCSV cardID cardName cardPower = "\n" ++ show cardID ++ "," ++ cardName ++ "," ++ show cardPower

rankingToCSV :: String -> Int -> String
rankingToCSV rankName rankPoints = "\n" ++ rankName ++ "," ++ show rankPoints

saveCardCSV :: Int -> String -> Int -> IO()
saveCardCSV cardID cardName cardPower = do
    let card = cardToCSV cardID cardName cardPower
    file <- openFile (head paths) AppendMode
    hPutStr file card
    hClose file

saveRankingCSV :: String -> Int -> IO()
saveRankingCSV rankName rankPoints = do
    let rank = rankingToCSV rankName rankPoints
    file <- openFile (paths !! 1) AppendMode
    hPutStr file rank
    hClose file