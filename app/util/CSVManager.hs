{-
    Módulo referente ao carregamento das cartas via CSV.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module CSVManager where

import Card ( Card(Card) )
import Text.CSV ( Record, parseCSV )
import System.IO.Unsafe (unsafePerformIO)

-- | Esta função retorna uma lista de caminhos para arquivos JSON.
paths :: [FilePath]
paths = ["app/data/csv/cards.csv",
         "app/data/csv/ranking.csv"]

-- | Esta função retorna o arquivo CSV selecionado como uma lista de Records.
getCSV :: Int -> [Record]
getCSV i = do
    let file = unsafePerformIO (readFile $ paths !! i)
    let csvFile = parseCSV (paths !! i) file
    either csvParseError csvParseDone csvFile
    where csvParseError csvFile = []
          csvParseDone = tail

-- | Esta função converte uma String em Int.
toInt :: String -> Int
toInt n = read n :: Int

-- | Esta função converte uma lista de Records em uma lista de cartas.
recordToCards :: [Record] -> [Card]
recordToCards [] = []
recordToCards (x:xs) = do
    let c = Card (toInt $ head x) (x!!1) (toInt $ x!!2)
    c : recordToCards xs

-- | Esta função retorna o conteúdo do arquivo CSV que contém as cartas como 
-- uma lista de cartas.
getCards :: [Card]
getCards = recordToCards (getCSV 0)