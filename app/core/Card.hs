{-
    Módulo referente a definição e funcionalidades das cartas
-}

{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Card where

-- | `Element` é uma String que indica o elemento da carta
type Element = String

-- | Este tipo representa uma carta, incluindo seu ID, elemento e nível de poder
data Card = Card {
    id :: Int,
    element :: Element,
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

-- | Dados dois elementos, determina qual deles é superior ao outro
winsAgainst :: Element -> Element -> Ordering
winsAgainst a b
    | a == b = EQ
    | a == "Fogo"     = if b == "Metal"    || b == "Natureza" then GT else LT
    | a == "Metal"    = if b == "Natureza" || b == "Terra"    then GT else LT
    | a == "Natureza" = if b == "Terra"    || b == "Água"     then GT else LT
    | a == "Terra"    = if b == "Água"     || b == "Fogo"     then GT else LT
    | a == "Água"     = if b == "Fogo"     || b == "Metal"    then GT else LT
    | otherwise = EQ
