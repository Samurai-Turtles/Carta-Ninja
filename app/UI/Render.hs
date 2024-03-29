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
import System.Info ( os )
import Ranking (formatRankingToScreen)
import Hammer (forgeScreen, mergeControll)
import Gameplay (getWinner)

-- | Esta função analisa o estado do jogo e realiza o print da respectiva tela.
action :: IO()
action = do
    let clear = if os == "mingw32" then "cls" else "clear"
    callCommand clear

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
    | state == "desafiante" = drawDesafiante
    | state == "batalha" = drawBatalha
    | state == "comparacao" = drawCompare
    | state == "vitoria" = drawVenceu
    | state == "derrota" = drawDerrota
    | state == "gameOver" = drawGameOver
    | otherwise = putStrLn ("\ESC[31m(UI): State not identified: '" ++ state ++ "' doesn't exist\ESC[0m")

-- | Esta função imprime a tela de menu.
drawMenu :: IO()
drawMenu = putStrLn (unlines scMenu)

-- | Esta função imprime a tela de ranking.
drawRank :: IO()
drawRank = do
    global <- getGlobalState

    let representation = map formatRankingToScreen (take 6 $ rankings global)
    let complete = take (138 - 23 * length representation) (cycle "=")

    let contentChar = mergeControll (length representation) [representation ++ [complete]]

    putStrLn (forgeScreen (unlines scRanking) (contentChar))

-- | Esta função imprime a tela de créditos.
drawCreditos :: IO()
drawCreditos = putStrLn (unlines scCreditos)

drawDesafiante :: IO()
drawDesafiante = putStrLn (unlines scDesafiante)

-- | Esta função imprime a tela de batalha.
drawBatalha :: IO()
drawBatalha = do
    battle <- getBattleState
    campaign <- getCampaignState
    let handRepresentation = currentCards (playerDeck battle)

    let contentChar =
            (fillNum (playerScore battle)) ++
            (usedElements (playerWinsByElement battle) ["FOGO","NATUREZA","ÁGUA","METAL","TERRA"]) ++
            (fillNum (cpuScore battle)) ++ (fillNum (lifes campaign)) ++ mergeControll 7 handRepresentation

    putStrLn (forgeScreen (unlines scBatalha) contentChar)

-- | Esta função imprime a tela de Comparação entre cartas.
drawCompare :: IO()
drawCompare = do
    battle <- getBattleState

    let usedCards =
            [playerDeck battle !! (length (playerDeck battle) -1),
             cpuDeck battle !! (length (cpuDeck battle) -1)]
    let out
          | cardWinner == 1 = forgeScreen (unlines scVenceuComparacao) mergeCards
          | cardWinner == -1 = forgeScreen (unlines scPerdeuComparacao) mergeCards
          | otherwise = ""
          where cardWinner = getWinner (head usedCards) (usedCards !! 1)
                mergeCards = mergeControll 7 [getCardStyle $ cardID (usedCards !! 0), getCardStyle $ cardID (usedCards !! 1)]

    putStrLn out

-- | Esta função imprime a tela de vitória.
drawVenceu :: IO()
drawVenceu = do
    campaign <- getCampaignState

    let contentChar =
            take (3 - length (show $ totalScore campaign)) (cycle "0") ++
            show (totalScore campaign)

    putStrLn (forgeScreen (unlines scVitoria) contentChar)

-- | Esta função imprime a tela de derrota.
drawDerrota :: IO()
drawDerrota = do
    campaign <- getCampaignState

    let contentChar =
            take (3 - length (show $ totalScore campaign)) (cycle "0") ++
            show (totalScore campaign) ++ fillNum (lifes campaign)

    putStrLn (forgeScreen (unlines scDerrota) contentChar)

-- | Esta função imprime a tela de gameOver.
drawGameOver :: IO()
drawGameOver = putStrLn (unlines scGameOver)

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

-- | Esta função seleciona a representação da carda, a partir de um dado id.
getCardStyle :: Int -> [String]
getCardStyle id = scCardsKanji !! (id - 1)

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