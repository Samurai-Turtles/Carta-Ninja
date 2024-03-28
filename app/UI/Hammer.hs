{-
    Módulo que forja telas que contém placeholder.
-}
module Hammer where

-- | Função que faz o replace dos elementos de place holders.
-- Lê todos os caracteres da tela atual e substitui o caracter caso ele seja um place holder.
-- Espera um background, contendo place holders do caracter #, e uma String com as informações
-- a serem usadas para replace.
forgeScreen :: String ->  String -> String
forgeScreen "" _ = ""
forgeScreen (h:t) especialChar
    | h == '#' = take 1 especialChar ++ forgeScreen t (drop 1 especialChar)
    | otherwise = h : forgeScreen t especialChar

-- | Esta função faz o merge das representações contidas em uma matriz.
--  A saída está normalizada para a função `forgeScreen`.
--  Espera um inteiro que é o index da última linha das representações,
--  além de uma matriz com as representações, com o mesmo length.
mergeControll :: Int -> [[String]] -> String
mergeControll (-1) _ = ""
mergeControll lineIndex arr =  mergeControll (lineIndex - 1) arr ++ mergeLine lineIndex (length arr - 1) arr

-- | Esta função faz o merge das linhas de cada elemento da matriza, faz isso
-- utilizando como base um inteiro que representa a linha a ser olhada e que é
-- passado como argumento.
mergeLine :: Int -> Int -> [[String]] -> String
mergeLine _ (-1) _ = ""
mergeLine lineIndex idx arr = mergeLine lineIndex (idx - 1) arr ++ (arr !! idx) !! lineIndex