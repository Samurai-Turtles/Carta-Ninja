{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Essas extensões de linguagem estão aqui para que o linter
-- não "reclame" do estilo sintático escolhido.
{-# HLINT ignore "Use head" #-}


module Render where

-- TODO lembrar de mudar os imports para que importem
-- somente as funções necessárias.
import ReadData
import SpritesBase
import Hammer (forgeScreen)

-- | Esta função analisa o estado do jogo e realiza o print da respectiva tela.
action :: IO()
action
    | choice == "menu" = drawMenu
    | choice == "batalha" = drawBatalha
    | otherwise = putStrLn ("ERR.: Status : " ++ choice ++ "doesn't exist")
      where choice = screen getGeneralData

-- | Esta função imprime a tela de Menu.
drawMenu :: IO()
drawMenu = putStrLn (unlines scMenu)

-- | Esta função imprime a tela de batalha.
drawBatalha :: IO()
drawBatalha = do--putStrLn (unlines scBatalha)
    let contentChar = (take 2 scoreInfo) ++ 
                    (usedElements (h_vitoriasElementos getGameplayData) ["FOGO","ÁGUA","NATUREZA","METAL","TERRA"]) 
                    ++ (drop 2 scoreInfo) ++ qtLifeInfo

    putStrLn (forgeScreen (unlines scBatalha) contentChar)


-- Funções que preparam as informações de replace de placeholder:

-- | Esta função seleciona a representação das cartas para 
-- forjar os espaços da mão do jogador.
currentCards :: [[String]]
currentCards = do 
    let hand = take 5 (h_deck getGameplayData)
    [scCards !! ((identifier (hand !! 0))-1), 
     scCards !! ((identifier (hand !! 1))-1),
     scCards !! ((identifier (hand !! 2))-1),
     scCards !! ((identifier (hand !! 3))-1),
     scCards !! ((identifier (hand !! 4))-1)
     ]

-- | Esta função prepara as entradas de substituição de cada place holder de score.
scoreInfo :: String
scoreInfo = do
    let scorePlayer = if h_score getGameplayData <= 9 then "0" ++ show (h_score getGameplayData) else show (h_score getGameplayData)
    let scoreComputador = if c_score getGameplayData <= 9 then "0" ++ show (c_score getGameplayData) else show (h_score getGameplayData)

    scorePlayer ++ scoreComputador

-- | Esta função prepara as entradas de substituição para cada place holder da quantidade de vida.
qtLifeInfo :: String
qtLifeInfo = if vidas getCampaignData <= 9 then "0" ++ show (vidas getCampaignData) else show (vidas getCampaignData)

-- | Esta função prepara as entradas de substituição para cada place holder dos elementos vitoriosos.
usedElements :: [Bool] -> [String] -> String
usedElements [] [] = ""
usedElements [] _ = ""
usedElements _ [] = ""
usedElements (h:t) (x:xs) = if h then x ++ usedElements t xs else " " ++ usedElements t xs