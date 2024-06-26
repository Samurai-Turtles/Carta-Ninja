{-
    Módulo de execução do loop geral de jogo.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GameLoop where

import Bot
import Tips
import State
import Ranking
import Gameplay 
import StateManager
import Deck (playCard)
import Render (action)
import Helpers (organizeRankings)

import System.Process
import System.Info (os)
import System.Exit (die)
import Data.Char (toUpper)
import CSVManager (getCards)


-- | Inicia o loop principal do jogo e estabelece os dados iniciais. Após 
-- iniciado, chama a função que inicia o loop do menu.
initLoop :: IO()
initLoop = do
    let clear = if os == "mingw32" then "cls" else "clear"
    callCommand clear

    menuLoop

-- | Esta executa o loop sobre as possibilidades de escolha na tela de menu.
-- O loop é repetido até o jogador digitar um valor possível para escolha.
menuLoop :: IO()
menuLoop = do
    localUpdateScreen "menu"
    action
    input <- getLine

    menuResolve $ validInput ["I", "R", "C", "S"] input

-- | Esta função chama o próximo loop a ser executado de acordo com a escolha do
-- jogador. Caso não seja uma escolha válida, o loop atual é chamado mais uma vez.
menuResolve :: String -> IO()
menuResolve "I" = nameInput
menuResolve "R" = rankingLoop
menuResolve "C" = creditLoop
menuResolve "S" = do
    if os == "mingw32" then callCommand "cls" else callCommand "clear"
    die "Carta-Ninja Encerrado"
menuResolve _ = menuLoop

-- | Esta função executa o loop sobre as possibilidades de escolha na tela de ranking.
-- O loop é repetido até o jogador digitar um valor possível para escolha.
rankingLoop :: IO()
rankingLoop = do
    localUpdateScreen "ranking"
    action

    input <- getLine
    let choice = validInput ["V"] input
    if choice /= "" then menuLoop else rankingLoop

-- | Esta função executa o loop sobre as possibilidades de escolha na tela de créditos.
-- O loop é repetido até o jogador digitar um valor possível para escolha.
creditLoop :: IO()
creditLoop = do
    localUpdateScreen "creditos"
    action

    input <- getLine
    let choice = validInput ["V"] input
    if choice /= "" then menuLoop else creditLoop

-- | Esta função executa o loop sobre a requisição do nome do desafiante.
-- O loop é repetido até o jogador digitar um nome válido.
nameInput :: IO()
nameInput = do 
    localUpdateScreen "desafiante"
    resetCampaignState
    action 
    
    input <- getLine
    if input == "" 
        then nameInput 
        else do
            updatePlayerName input
    
            initBattleData
            battleLoop

-- | Esta função executa o loop sobre o estágio de batalha entre o jogador e o bot.
-- O loop é repetido até a partida encerrar em caso de vitória, de derrota, de empate ou 
-- caso o jogador não digite uma opção válida dentre as possíveis.
battleLoop :: IO()
battleLoop = do
    battle <- getBattleState

    let victoryCheck = verifyVictory battle

    if victoryCheck /= 0 then comparationResolve victoryCheck 
    else do 
        localUpdateScreen "batalha"
        action

        input <- getLine
        let playerDecide = validInput ["1", "2", "3", "4", "5", "6", "7", "8", "D"] input

        battleResolve battle playerDecide

        newBattleStage <- getBattleState
        let winner = 
                if verifyNullifyElemSpecialCardUse newBattleStage then 
                    getWinnerNullElement (last $ playerDeck newBattleStage) (last $ cpuDeck newBattleStage)
                else
                    getWinner (last $ playerDeck newBattleStage) (last $ cpuDeck newBattleStage)

        comparationLoop winner

-- | Esta função executa o loop sobre a escolha do player no estágio de batalha, permitindo
-- a ação das cartas especiais, das cartas comuns ou da dica.
battleResolve :: BattleState -> String -> IO()
battleResolve battle "6" = specialResolve battle 6
battleResolve battle "7" = specialResolve battle 7
battleResolve battle "8" = specialResolve battle 8
battleResolve battle "D" = tipResolve battle
battleResolve _ "" = battleLoop
battleResolve battle choice = do
    campaign <- getCampaignState

    let botChoice = makeChoice battle (beltLevel campaign)

    let newPlayerDeck = playCard (playerDeck battle) $ read choice - 1
    let newBotDeck = playCard (cpuDeck battle) botChoice

    let modifiedData = BattleState {
                currentRound = currentRound battle,

                playerScore = playerScore battle,
                playerStreak = playerStreak battle,
                playerWinsByElement = playerWinsByElement battle,
                playerDeck = newPlayerDeck,

                cpuScore = cpuScore battle,
                cpuStreak = cpuStreak battle,
                cpuWinsByElement = cpuWinsByElement battle,
                cpuDeck = newBotDeck,

                specialDeck = specialDeck battle,
                specialCardInUse = specialCardInUse battle,
                tipAvailability = tipAvailability battle
            }

    writeBattleState modifiedData

    if tipAvailability battle == "PRESSIONE [D] PARA USAR A DICA" then updateTipOf $ tipAvailability battle
    else updateTipOf "DICA USADA"

-- | Esta função resolve a utilização de uma carta especial, quando disponível.
specialResolve :: BattleState -> Int -> IO()
specialResolve battle specialNum = 
        if verifySpecialCardAvailability battle == 1 then do
            useSpecialCard specialNum
            battleLoop
        else battleLoop

-- | Esta função resolve a utilização da funcionalidade de dicas.
tipResolve :: BattleState -> IO()
tipResolve battle = do
    if tipAvailability battle == "PRESSIONE [D] PARA USAR A DICA" then do
        updateTipOf $ giveTip battle
        battleLoop
    else do
        battleLoop

-- | Esta função executa o loop sobre o estágio de comparação das cartas.
-- Caso haja definição na batalha, o próximo estágio declarado será o de rosolução
-- de uma comparação.
comparationLoop :: Int -> IO()
comparationLoop winner = do
    battle <- getBattleState
    localUpdateScreen "comparacao"
    action
    
    if winner == 1 then 
        updateScoreOf winner (last $ playerDeck battle)
    else
        updateScoreOf winner (last $ cpuDeck battle)

    _ <- getLine

    battleLoop

-- | Esta função resolve a situação do jogador para a batalha.
comparationResolve :: Int -> IO()
comparationResolve (-2) = battleDrawLoop
comparationResolve (-1) = battleDefeatLoop
comparationResolve 1 = battleVictoryLoop

-- | Esta função executa o loop sobre o estágio de empate em uma partida.
-- Nesse caso, a partida é repetida sem problemas.
battleDrawLoop :: IO()
battleDrawLoop = do
    battle <- getBattleState
    updatePlayerCampaignScore $ playerScore battle

    localUpdateScreen "empate"
    action

    _ <- getLine

    initBattleData
    battleLoop

-- | Esta função executa o loop sobre o estágio de derrota em uma partida.
-- Nesse caso, a partida é repetida, caso o player possua vidas restantes;
-- a campanha é encerrada, caso o player não tenha vidas restantes.
battleDefeatLoop :: IO()
battleDefeatLoop = do
    battle <- getBattleState
    campaign <- getCampaignState
    updatePlayerCampaignScore $ playerScore battle

    if lifes campaign > 0 then do
        localUpdateScreen "derrota"
        action
        _ <- getLine
        initBattleData
        updatePlayerLife (-1)
        battleLoop
    else do 
        localUpdateScreen "gameOver"
        action
        _ <- getLine
        updateRanking
        rankingLoop

-- | Esta função executa o loop sobre o estágio de vitória em uma partida.
-- Nesse caso, o jogador avança de nível, caso ele esteja em até um nível abaixo
-- do último; o jogo encerra, caso todos os níveis tiverem sido jogados.
battleVictoryLoop :: IO()
battleVictoryLoop = do
    battle <- getBattleState
    campaign <- getCampaignState
    updatePlayerCampaignScore $ playerScore battle

    if playerScore battle >= 25 then updatePlayerLife 1 else updatePlayerLife 0

    if beltLevel campaign >= 5 then do
        localUpdateScreen "gameClear"
        action
        _ <- getLine
        updateRanking
        rankingLoop
    else do
        localUpdateScreen "vitoria"
        action
        _ <- getLine
        levelUpPlayer
        initBattleData
        battleLoop

------------------------------ Funções Auxiliares ------------------------------

-- | Inicia os dados de um estágio de batalha utilizando um padrão comum.
initBattleData :: IO()
initBattleData = do
    let deckDefault = getCards
    playerRandomDeck <- shuffle deckDefault
    cpuRandomDeck <- shuffle deckDefault

    let modifiedData = BattleState {
                currentRound = 1,

                playerScore = 0,
                playerStreak = 0,
                playerWinsByElement = [False, False, False, False, False],
                playerDeck = playerRandomDeck,

                cpuScore = 0,
                cpuStreak = 0,
                cpuWinsByElement = [False, False, False, False, False],
                cpuDeck = cpuRandomDeck,

                specialDeck = ["swapInDeck", "nullifyElement", "swapBetweenDecks"],
                specialCardInUse = False,
                tipAvailability = "PRESSIONE [D] PARA USAR A DICA"
            }

    writeBattleState modifiedData

-- | Adiciona o ranking finalizado do jogador atual.
updateRanking :: IO()
updateRanking = do
    global <- getGlobalState
    campaign <- getCampaignState

    let newRanking = Ranking {points = totalScore campaign, name = playerName campaign}

    let modifiedData = 
            GlobalState {
                screen = screen global, 
                rankings = organizeRankings $ newRanking : rankings global
                }
    writeGlobalState modifiedData

-- | Função que denota a nova tela que representa o loop atual de jogo. Recebe 
-- um argumento que representa a nova tela a ser escrita. 
localUpdateScreen :: String -> IO()
localUpdateScreen newScreen = do
    currentGlobals <- getGlobalState
    writeGlobalState $ GlobalState {screen = newScreen, rankings = rankings currentGlobals}

-- | A partir de uma lista de entradas válidas para a tela atual e uma String que
-- é a entrada, esta função retorna a última entrada válida digitada pelo player
-- antes de um [enter]. Palavra vazia se não houver entrada válida.
validInput :: [String] -> String -> String
validInput validKeys input = do
    let inputsPossible = filter (`elem` validKeys) $ splitInput input

    if not (null inputsPossible) then last inputsPossible else ""

-- | Separa os elementos de uma String e os coloca em letra caixa alto, isso
-- quando possível.
splitInput :: String -> [String]
splitInput "" = []
splitInput (h:t) = [toUpper h] : splitInput t