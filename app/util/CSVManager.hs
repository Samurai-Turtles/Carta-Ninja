{-
    Módulo referente ao carregamento das cartas via CSV.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module CSVManager where

import Card
import Ranking
import Text.CSV
import System.IO.Unsafe (unsafePerformIO)
import System.IO

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

-- | Esta função converte uma lista de Records em uma lista de rankings.
recordToRanking :: [Record] -> [Ranking]
recordToRanking [] = []
recordToRanking (x:xs) = do
    let r = Ranking (head x) (toInt $ x !! 1)
    r : recordToRanking xs

-- | Esta função retorna o conteúdo do arquivo CSV que contém as cartas como 
-- uma lista de cartas.
getCards :: [Card]
getCards = recordToCards (getCSV 0)

-- | Esta função retorna o conteúdo do arquivo CSV que contém os rankings como 
-- uma lista de rankings.
getRanking :: [Ranking]
getRanking = recordToRanking (getCSV 1)

-- | Essa função recebe um nome e uma quantidade de pontos e converte esses 
-- argumentos em uma String em formato CSV.
rankingToCSV :: String -> Int -> String
rankingToCSV rankName rankPoints = "\n" ++ rankName ++ "," ++ show rankPoints

-- | Essa função salva um ranking selecionado no arquivo CSV que contém os 
-- rankings.
saveRankingCSV :: String -> Int -> IO()
saveRankingCSV rankName rankPoints = do
    let rank = rankingToCSV rankName rankPoints
    file <- openFile (paths !! 1) AppendMode
    hPutStr file rank
    hClose file