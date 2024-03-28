{-
    Módulo referente ao bot do jogo
-}

module Bot where

import Card
import System.Random

-- Gera um número aleatório a partir da seed até o numero max, recebidos como parâmetro
-- Semente -> Limite superior -> Numero aleatório
nRandom :: Int -> Int -> Int
nRandom seed max =  mod (31237 * seed) max

-- Recebe uma lista de cartas e uma lista de pesos e gera uma lista maior, onde cada carta é repetida de acordo com o peso
-- Lista de cartas -> Lista de pesos -> Lista maior de cartas
wHand :: [a] -> [Int] -> [a]
wHand [] [] = []
wHand (x:xs) (y:ys) = replicate y x ++ wHand xs ys

-- Recebe uma lista de cartas e uma lista de pesos e consegue fazer a escolha da carta de acordo com os pesos
-- Pontuação do jogador -> Lista de Cartas -> Lista de Pesos -> Carta
makeChoice :: Int -> [a] -> [Int] -> a
makeChoice points cards weight = do
    let hand = wHand cards weight
    let len = length (wHand cards weight)
    hand !! nRandom points (len-1)

-- Função para fazer o shuffle de um array 
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))
