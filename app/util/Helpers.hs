{-
    Módulo de funções utilitárias e genéricas.
-}

module Helpers where

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
