{-
    Módulo referente ao carregamento das cartas via CSV
-}

{-# LANGUAGE OverloadedStrings #-}

module CardLoader where

import Card
import Text.CSV
import System.IO.Unsafe (unsafePerformIO)

pathCard :: FilePath
pathCard = "app/core/csv/cards.csv"

csvParseError :: p -> [a]
csvParseError csvFile = []

csvParseDone :: [a] -> [a]
csvParseDone = tail

getCardsCSV :: [Record]
getCardsCSV = do
    let file = unsafePerformIO (readFile pathCard)
    let csvFile = parseCSV pathCard file
    either csvParseError csvParseDone csvFile

toInt :: String -> Int
toInt n = read n :: Int

recordToCards :: [Record] -> [Card]
recordToCards [] = []
recordToCards (x:xs) = do
    let c = Card (toInt $ head x) (x!!1) (toInt $ x!!2)
    c : recordToCards xs
