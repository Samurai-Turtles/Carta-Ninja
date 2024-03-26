{-
    Módulo referente ao bot do jogo
-}

module Bot where

import Card
import System.Random (randomRIO)


makechoice :: [Card] -> [Int] -> Card
make_choice hand weight = do
    let w_hand = weight_hand 0 hand weight
        len = length w_hand

    return (hand !! (randomRIO (0, len - 1)))

weight_hand :: Int -> [Card] -> [Int] -> [Int]
weight_hand i [] [] = []
weight_hand 5   = []
weight_hand i cards weights =
    case elem of
        "F" -> replicate (weights !! 0) i ++ weight_hand (i + 1) cards weights
        "A" -> replicate (weights !! 1) i ++ weight_hand (i + 1) cards weights
        "M" -> replicate (weights !! 2) i ++ weight_hand (i + 1) cards weights
        "N" -> replicate (weights !! 3) i ++ weight_hand (i + 1) cards weights
        "T" -> replicate (weights !! 4) i ++ weight_hand (i + 1) cards weights
  where elem = element (cards !! i)

shuffle :: [a] -> IO [a]
shuffle [] = []
shuffle xs = do
    let len = length xs

    idx <- randomRIO (0, len - 1)
    let val = xs !! idx
    return ([val] ++ shuffle (removeFirstEqualTo val xs))

removeFirstEqualTo :: Eq a => a -> [a] -> [a]
removeFirstEqualTo  [] = []
removeFirstEqualTo x (y:ys)
    | x == y    = ys
    | otherwise = [y]  removeFirstEqualTo x ys