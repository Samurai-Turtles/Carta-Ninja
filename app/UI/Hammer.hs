module Hammer where

-- | função que faz o replace dos elementos de place holders.
-- Lê todos os caracteres da tela atual e substitui o caracter caso ele seja um place holder.
-- Espera um background, contendo place holders do caracter #, e uma String com as informações
-- a serem usadas para replace.
forgeScreen :: String ->  String -> String
forgeScreen "" _ = ""
forgeScreen (h:t) especialChar
    | h == '#' = take 1 especialChar ++ forgeScreen t (drop 1 especialChar)
    | otherwise = [h] ++ forgeScreen t especialChar