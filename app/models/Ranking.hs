{-
    Módulo referente a definição e funcionalidades dos Rankings
-}

{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ranking where

-- | Este tipo representa um Ranking, incluindo nome da campanha e a 
-- pontuação obtida nela
data Ranking = Ranking {
    name :: String,
    points :: Int
}

-- Exibe os dados do Ranking conforme o exemplo: `<name> - <points> pts.`
instance Show Ranking where
    show :: Ranking -> String
    show (Ranking name points) = name ++ " - " ++ show points ++ " pts."

-- Implementa igualdade de Rankings
-- Um Ranking é igual a outro quando seus nomes e pontuações forem iguais
instance Eq Ranking where
    (==) :: Ranking -> Ranking -> Bool
    a == b = (name a == name b) && (points a == points b)

-- Implementa comparação de Rankings
-- Um Ranking é maior que outro se sua pontuação for maior que a pontuação do
-- outro Ranking
instance Ord Ranking where
    compare :: Ranking -> Ranking -> Ordering
    compare a b = points a `compare` points b

formatRankingToScreen :: Ranking -> String
formatRankingToScreen r =
    if length (name r) < 20 then name r ++ take (20 - length (name r)) (cycle " ") ++ show (points r)
    else take 20 (name r) ++ show (points r)