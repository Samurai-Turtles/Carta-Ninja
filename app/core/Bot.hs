{-
    Módulo referente ao bot do jogo
-}

module Bot where

import Card
import System.Random (randomRIO, Random (random))
import GHC.IO (unsafeDupablePerformIO)

-- Gera um número aleatório de 0 até b, recebido como parâmetro
nRandom :: Int -> Int
nRandom b = do
    let rand = randomRIO (0,b) :: IO Int
    typeConvert rand

-- Converte de tipo IO Int para tipo Int
typeConvert :: IO Int -> Int
typeConvert = unsafeDupablePerformIO

-- Recebe uma lista de cartas e uma lista de pesos e gera uma lista maior, onde cada carta é repetida de acordo com o peso
wHand :: [a] -> [Int] -> [a]
wHand [] [] = []
wHand (x:xs) (y:ys) = replicate y x ++ wHand xs ys

-- Recebe uma lista de cartas e uma lista de pesos e consegue fazer a escolha da carta de acordo com os pesos
makeChoice :: [a] -> [Int] -> a
makeChoice cards weight = do
    let hand = wHand cards weight
    let len = length (wHand cards weight)
    hand !! nRandom (len -1)

-- Função para fazer o shuffle de um array 
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

-- Remove a primeira aparição do elemento passado do deck
-- removeFirstEqualTo :: Eq a => a -> [a] -> [a]
-- removeFirstEqualTo  [] = []
-- removeFirstEqualTo x (y:ys)
--     | x == y    = ys
--     | otherwise = [y]  removeFirstEqualTo x ys