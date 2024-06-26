{-
    Módulo de funções utilitárias e genéricas.
-}

module Helpers where

import Ranking
import Data.List
import GHC.IO (unsafePerformIO)
import System.Random

-- | Essa função verifica se um índice está dentro de um intervalo
validateIndex :: Int -> (Int, Int) -> Bool
validateIndex index range = index >= fst range && index <= snd range

-- | Essa função move um elemento de um índice para o final da lista
pushElementToEnd :: Int -> [a] -> [a]
pushElementToEnd _ []  = []
pushElementToEnd _ [x] = [x]
pushElementToEnd index xs
    | index < 0 || index >= length xs = xs
    | otherwise = fst list ++ tail (snd list) ++ [head (snd list)]
        where
            list = splitAt index xs

-- | Essa função modifica o elemento de uma posição específica de uma lista
replaceAtIndex :: [a] -> a -> Int -> [a]
replaceAtIndex [] _ _ = []
replaceAtIndex (h : t) item index
    | not (validateIndex index (0, length (h:t) - 1)) = h : t
    | index == 0 = item : t
    | otherwise = h : replaceAtIndex t item (index - 1)

-- | Essa função pega uma lista e um elemento e retorna uma cópia da lista
-- original sem esse elemento se ele existir nela, caso contrário, só retorna
-- a lista original sem mudanças.
removeElementList :: Eq a => [a] -> a -> [a]
removeElementList [] _ = []
removeElementList (h:t) item
    | item == h = t
    | otherwise = h : removeElementList t item

-- | Esta função remove duplicatas de uma lista de Rankings.
removeDuplicateRankings :: [Ranking] -> [Ranking]
-- Utilizar apenas numa lista ordenada ao contrário de rankings.
removeDuplicateRankings = nubBy (\x y -> name x == name y)

-- | Esta função ordena uma lista ao contrário
sortReverse :: (Ord a) => [a] -> [a]
sortReverse = sortBy (flip compare)

-- | Esta função aplica a função de ordenamento reverso, seguida da função de
-- remover duplicatas numa lista de Rankings. 
organizeRankings :: [Ranking] -> [Ranking]
organizeRankings = removeDuplicateRankings . sortReverse

-- | Esta função usa 2 parâmetros, em que o primeiro é o limite inferior e o segundo,
-- o limite superior, retornando um número randômico nesse intervalo fechado dado.
generateRandom :: Int -> Int -> Int
generateRandom a b = unsafePerformIO (getStdRandom (randomR(a, b)))