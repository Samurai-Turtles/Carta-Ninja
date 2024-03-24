{-
    Módulo referente a definição e funcionalidades das cartas
-}

{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Card where

-- | Este tipo representa uma carta, incluindo seu ID, elemento e nível de poder
data Card = Card {
    id :: Int,
    element :: String,
    power :: Int
}

-- Exibe os dados da carta conforme o exemplo: `{id} [<Elemento> (<Poder>)]`
instance Show Card where
    show :: Card -> String
    show (Card id element power) = 
        "{" ++ show id ++ "}" ++ " [" ++ element ++ " (" ++ show power ++ ")]"

-- Implementa igualdade de cartas
-- Uma carta é igual a outra se, e somente sem, tiverem o mesmo elemento e poder
instance Eq Card where
    (==) :: Card -> Card -> Bool
    a == b = (element a == element b) && (power a == power b)

-- Implementa comparação de cartas
-- Uma carta vence outra se seu elemento for dominante contra a outra carta,
-- ou se seu poder for superior (dado que sejam do mesmo elemento)
instance Ord Card where
    compare :: Card -> Card -> Ordering
    compare a b
        | element a == element b = power a `compare` power b
        | otherwise = element a `winsAgainst` element b

-- | Dados dois elementos, determina qual deles é dominante sobre o outro
winsAgainst :: String -> String -> Ordering
winsAgainst a b
    | a == b = EQ
    | a == "fire"   = if b == "metal"  || b == "nature" then GT else LT
    | a == "metal"  = if b == "nature" || b == "earth"  then GT else LT
    | a == "nature" = if b == "earth"  || b == "water"  then GT else LT
    | a == "earth"  = if b == "water"  || b == "fire"   then GT else LT
    | a == "water"  = if b == "fire"   || b == "metal"  then GT else LT
    | otherwise = EQ
