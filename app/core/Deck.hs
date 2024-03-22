{-
 - Módulo referente às funcionalidades relacionadas ao Deck de cartas do jogo.
 -}

module Deck (
    pushToEnd
) where

-- | Esta função pega uma carta específica do Deck e move até o final da lista
pushToEnd :: Int -> [a] -> [a]
pushToEnd _ [] = []
pushToEnd _ [x] = [x]
pushToEnd index xs
    | index < 0 || index >= length xs = xs
    | otherwise = fst list ++ tail(snd list) ++ (head(snd list)):[]
        where list = splitAt index xs
