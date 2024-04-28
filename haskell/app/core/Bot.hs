{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Bot where

import Card
import State
import System.Random
import Helpers (replaceAtIndex)

-- Battlestate -> nível de dificuldade (dl) -> elemento escolhido
makeChoice :: BattleState -> Int -> Int
makeChoice bstate dl = do
    let seed = playerScore bstate
    let weight = wDeck (playerDeck bstate) dl
    let cpu_hand = take 5 (cpuDeck bstate)
    let w_cpu_hand = wHand cpu_hand weight
    let len = length w_cpu_hand

    w_cpu_hand !! nRandom seed (len-1)


wDeck :: [Card] -> Int -> [Int] -- F N A M T
wDeck deck dl = wDeckRecursive deck dl (replicate 5 $ 1+(6*dl))

wDeckRecursive :: [Card] -> Int -> [Int] -> [Int]
wDeckRecursive [] _ w = w
wDeckRecursive (a:as) dl w =
    case elem of
        -- Para cada elemento do tipo indicado presente no deck do jogador, deve-se aumentar o peso de 2 elementos e didx_to_decreaseinuir de 2
        -- Para elem == F, aumentar A e T (posições 2 e 4) e diminuir N e M (1 e 3)
        "fire" -> wDeckRecursive as dl (updateWeight dl [2,4,1,3] w)
        -- Para elem == N, aumentar F e M (posições 0 e 3) e diminuir A e T (2 e 4)
        "nature" -> wDeckRecursive as dl (updateWeight dl [0,3,2,4] w)
        -- Para elem == A, aumentar N e T (posições 1 e 4) e diminuir F e M (0 e 3)
        "water" -> wDeckRecursive as dl (updateWeight dl [1,4,0,3] w)
        -- Para elem == M, aumentar F e A (posições 0 e 2) e diminuir N e T (1 e 4)
        "metal" -> wDeckRecursive as dl (updateWeight dl [0,2,1,4] w)
        -- Para elem == T, aumentar N e M (posições 1 e 3) e diminuir F e A (0 e 2)
        "earth" -> wDeckRecursive as dl (updateWeight dl [1,3,0,2] w)
        
        _ -> [-7]
    where elem = element a

updateWeight :: Int -> [Int] -> [Int] -> [Int]
updateWeight dl [idx_to_increase1, idx_to_increase2, idx_to_decrease1, idx_to_decrease2] weight = do
    let value_to_increase1 = (weight !! idx_to_increase1) + dl
    let w1 = updateArray weight idx_to_increase1 value_to_increase1
    let value_to_increase2 = (weight !! idx_to_increase2) + dl
    let w2 = updateArray w1 idx_to_increase2 value_to_increase2
    let value_to_decrase1 = (weight !! idx_to_decrease1) + dl
    let w3 = updateArray w2 idx_to_decrease1 value_to_decrase1
    let value_to_decrase2 = (weight !! idx_to_decrease2) + dl
    updateArray w3 idx_to_decrease2 value_to_decrase2


updateArray :: [Int] -> Int -> Int -> [Int]
updateArray arr idx newVal = replaceAtIndex arr newVal idx

-- Gera um número aleatório a partir da seed até o numero max, recebidos como parâmetro
-- Semente -> Limite superior -> Numero aleatório
nRandom :: Int -> Int -> Int
nRandom seed max =  mod (31237 * seed) max

-- A ordem agora é FNAMT
-- O que esta função faz é chamar a função recursiva wHandRecursive
-- recebe uma lista de cartas (a mão do player) -> uma lista de pesos (o peso das cartas de fogo está na posição 0, de natureza em 1 e assim por diante)
-- retorna uma lista de inteiros, onde os índices [0...4] são repetidos, por exemplo, se a mão do computador for: [F1,A3,A1,N1,T2] e os pesos forem: [2,2,3,1,2] o retorno será:
-- [0,0,1,1,1,2,2,2,3,3,4,4] -> os dois zeros no início fazem referência à carta na posição 0 (F1) que é de fogo, e fogo tem peso 2, o mesmo vale para os outros elementos
wHand :: [Card] -> [Int] -> [Int]
wHand c w = wHandRecursive 0 c w

wHandRecursive :: Int -> [Card] -> [Int] -> [Int]
wHandRecursive 4 [] [] = []
wHandRecursive 4 _ _ = []
wHandRecursive i cards weights =
    case elem of
        "fire" -> replicate (weights !! 0) i ++ wHandRecursive (i + 1) cards weights
        "nature" -> replicate (weights !! 1) i ++ wHandRecursive (i + 1) cards weights
        "water" -> replicate (weights !! 2) i ++ wHandRecursive (i + 1) cards weights
        "metal" -> replicate (weights !! 3) i ++ wHandRecursive (i + 1) cards weights
        "earth" -> replicate (weights !! 4) i ++ wHandRecursive (i + 1) cards weights
        _ -> [-7]
    where elem = element (cards !! i)

-- Função para fazer o shuffle de um array 
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))