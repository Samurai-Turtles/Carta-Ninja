module Render where

import ReadData
import SpritesBase

-- | Esta função analisa o estado do jogo e realiza o print da respectiva tela.
action :: IO()
action 
    | choice == "menu" = drawMenu
    | choice == "batalha" = drawBatalha 
    | otherwise = putStrLn ("ERR.: Status : " ++ choice ++ "doesn't exist")
      where choice = screen getGeneralData

-- | Esta função imprime a tela de Menu.
drawMenu :: IO()
drawMenu = putStrLn (unlines (scMenu))

-- | Esta função imprime a tela de batalha.
drawBatalha :: IO() 
drawBatalha = putStrLn (unlines (scBatalha))


-- Funções que preparam as informações de replace de placeholder:

-- | Esta função prepara as entradas de substituição de cada place holder de score.
scoreInfo :: String
scoreInfo = do
    let scorePlayer = if (h_score getGameplayData) <= 9 then "0" ++ show (h_score getGameplayData) else show (h_score getGameplayData)
    let scoreComputador = if (c_score getGameplayData) <= 9 then "0" ++ show (c_score getGameplayData) else show (h_score getGameplayData)
    
    scorePlayer ++ scoreComputador

-- | Esta função prepara as entradas de substituição para cada place holder da quantidade de vida.
qtLifeInfo :: String
qtLifeInfo = if (vidas getCampaignData) <= 9 then "0" ++ show (vidas getCampaignData) else show (vidas getCampaignData)

-- | Esta função prepara as entradas de substituição para cada place holder dos elementos vitoriosos.
usedElements :: String
usedElements = fogo ++ agua ++ natureza ++ metal ++ terra
    where 
        fogo = if (winningElements !! 0 ) then "FOGO" else " "
        agua = if (winningElements !! 1 ) then "ÁGUA" else " "
        natureza = if (winningElements !! 2 ) then "NATUREZA" else " "
        metal = if (winningElements !! 3 ) then "METAL" else " "
        terra = if (winningElements !! 4 ) then "FOGO" else " "
        winningElements = h_vitoriasElementos getGameplayData
