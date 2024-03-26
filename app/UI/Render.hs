{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Essas extensões de linguagem estão aqui para que o linter
-- não "reclame" do estilo sintático escolhido.
{-# HLINT ignore "Use head" #-}


module Render where

-- TODO lembrar de mudar os imports para que importem
-- somente as funções necessárias.
import SpritesBase
import Hammer (forgeScreen, concatanateCards)

-- | Esta função analisa o estado do jogo e realiza o print da respectiva tela.
action :: IO()
action
    | choice == "menu" = drawMenu
    | choice == "ranking" = drawRank
    | choice == "creditos" = drawCreditos
    | choice == "batalha" = drawBatalha
    | choice == "venceu" = drawVenceu
    | choice == "derrota" = drawDerrota
    | otherwise = putStrLn ("\ESC[31m(UI): State not identified: '" ++ choice ++ "' doesn't exist\ESC[0m")
      where choice = screen getGeneralData

-- | Esta função imprime a tela de menu.
drawMenu :: IO()
drawMenu = putStrLn (unlines scMenu)

-- | Esta função imprime a tela de ranking.
drawRank :: IO()
drawRank = putStrLn (unlines scRanking)

-- | Esta função imprime a tela de créditos.
drawCreditos :: IO()
drawCreditos = putStrLn (unlines scCreditos)

-- | Esta função imprime a tela de batalha.
drawBatalha :: IO()
drawBatalha = do
    let contentChar = (take 2 scoreInfo) ++ 
                    (usedElements (h_vitoriasElementos getGameplayData) ["FOGO","ÁGUA","NATUREZA","METAL","TERRA"]) 
                    ++ (drop 2 scoreInfo) ++ qtLifeInfo ++ concatanateCards 7 currentCards

    putStrLn (forgeScreen (unlines scBatalha) contentChar)

-- | Esta função imprime a tela de vitória.
drawVenceu :: IO()
drawVenceu = putStrLn (unlines scVitoria)

-- | Esta função imprime a tela de derrota.
drawDerrota :: IO()
drawDerrota = putStrLn (unlines scDerrota)

-- Funções que preparam as informações de replace de placeholder:

-- | Esta função seleciona a representação das cartas para 
-- forjar os espaços da mão do jogador.
currentCards :: [[String]]
currentCards = do 
    let hand = take 5 (h_deck getGameplayData)
    [scCardsKanji !! ((identifier (hand !! 0))-1), 
     scCardsKanji !! ((identifier (hand !! 1))-1),
     scCardsKanji !! ((identifier (hand !! 2))-1),
     scCardsKanji !! ((identifier (hand !! 3))-1),
     scCardsKanji !! ((identifier (hand !! 4))-1)
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
usedElements (h:t) (x:xs) = do 
    let blankElement = take (length x) (cycle " ")
    if h then x ++ usedElements t xs else blankElement ++ usedElements t xs