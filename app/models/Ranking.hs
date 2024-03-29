{-
    Módulo referente a definição e funcionalidades dos Rankings
-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ranking where
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )

-- | Este tipo representa um Ranking, incluindo nome da campanha e a 
-- pontuação obtida nela
data Ranking = Ranking {
    name :: String,
    points :: Int
} deriving (Generic)

instance FromJSON Ranking
instance ToJSON Ranking

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
    if length (name r) < 20 then take (20 - length (name r)) (cycle " ") ++ name r ++ take (3 - length (show (points r))) (cycle " ") ++ show (points r)
    else take 20 (name r) ++ take (3 - length (show (points r))) (cycle " ") ++ show (points r)