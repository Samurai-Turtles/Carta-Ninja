{-
    Módulo referente ao carregamento das cartas via CSV
-}

{-# LANGUAGE OverloadedStrings #-}

module CSVManager where

import Card
import Ranking
import Text.CSV
import System.IO.Unsafe (unsafePerformIO)

paths :: [FilePath]
paths = ["app/core/csv/cards.csv",
         "app/core/csv/ranking.csv"]

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
