{-
    Módulo referente às funcionalidades relacionadas ao Deck de cartas do jogo.
 -}

module Deck (
    playCard
) where

import Card

-- | Esta função recebe o índice da carta jogada e retorna o Deck
-- após a jogada ser feita (com a carta jogada no final)
playCard :: [Card] -> Int -> [Card]
playCard cards index =
    if validateIndex index
        then pushToEnd index cards
        else []

-- | Esta função recebe o índice de uma carta do Deck e retorna o
-- Deck com a carta selecionada na última posição
pushToEnd :: Int -> [Card] -> [Card]
pushToEnd _ [] = []
pushToEnd _ [x] = [x]
pushToEnd index xs
    | index < 0 || index >= length xs = xs
    | otherwise = fst list ++ tail (snd list) ++ [head (snd list)]
        where list = splitAt index xs

-- | Essa função valida o `index` passado pelo jogador para selecionar uma carta.  
-- O `index` será considerado válido se `0 <= index < 5`
validateIndex :: Int -> Bool
validateIndex index = index >= 0 && index < 5