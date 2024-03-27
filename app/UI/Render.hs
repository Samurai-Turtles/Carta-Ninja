{-
    Módulo referente a impressão da tela atual.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Essas extensões de linguagem estão aqui para que o linter
-- não "reclame" do estilo sintático escolhido.
{-# HLINT ignore "Use head" #-}


module Render where

-- TODO lembrar de mudar os imports para que importem
-- somente as funções necessárias.
import Card
import State 
import SpritesBase
import StateManager
import System.Process
import Hammer (forgeScreen, concatanateCards)

-- | Esta função analisa o estado do jogo e realiza o print da respectiva tela.
action :: IO()
action = do
    callCommand "clear"

    global <- getGlobalState
    let state = screen global

    selectDraw state
    
-- | Esta função seleciona a função `draw` responsável pela impressão da tela, 
-- tomado por base o estado atual.
selectDraw :: String -> IO()
selectDraw state
    | state == "menu" = drawMenu
    | state == "ranking" = drawRank
    | state == "creditos" = drawCreditos
    | state == "batalha" = drawBatalha
    | state == "vitoria" = drawVenceu
    | state == "derrota" = drawDerrota
    | otherwise = putStrLn ("\ESC[31m(UI): State not identified: '" ++ state ++ "' doesn't exist\ESC[0m")

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
    battle <- getBattleState
    campaign <- getCampaignState
    let handRepresentation = currentCards (playerDeck battle)

    let contentChar = 
            (fillNum (playerScore battle)) ++ 
            (usedElements (playerWinsByElement battle) ["FOGO","NATUREZA","ÁGUA","METAL","TERRA"]) ++
            (fillNum (cpuScore battle)) ++ (fillNum (lifes campaign)) ++ concatanateCards 7 handRepresentation

    putStrLn (forgeScreen (unlines scBatalha) contentChar)

-- | Esta função imprime a tela de vitória.
drawVenceu :: IO()
drawVenceu = putStrLn (unlines scVitoria)

-- | Esta função imprime a tela de derrota.
drawDerrota :: IO()
drawDerrota = putStrLn (unlines scDerrota)

------------------------------ Funções Auxiliares ------------------------------

-- | Esta função seleciona a representação das cartas para 
-- forjar os espaços da mão do jogador.
currentCards :: [Card] -> [[String]]
currentCards cards = do 
    let hand = take 5 cards

    [scCardsKanji !! ((cardID (hand !! 0))-1), 
     scCardsKanji !! ((cardID (hand !! 1))-1),
     scCardsKanji !! ((cardID (hand !! 2))-1),
     scCardsKanji !! ((cardID (hand !! 3))-1),
     scCardsKanji !! ((cardID (hand !! 4))-1)]

-- | Esta função prepara o número para uma representação de dois digitos.
-- Preenche com um dígito zero após o primeiro número, complementando o espaço
-- restante, isso caso ele seja um número menor que 10. 
fillNum :: Int -> String
fillNum number = if number <= 9 then "0" ++ (show number) else show number

-- | Esta função prepara as entradas de substituição para cada place holder dos elementos vitoriosos.
usedElements :: [Bool] -> [String] -> String
usedElements [] [] = ""
usedElements [] _ = ""
usedElements _ [] = ""
usedElements (h:t) (x:xs) = do 
    let blankElement = take (length x) (cycle " ")
    if h then x ++ usedElements t xs else blankElement ++ usedElements t xs