{-
    Módulo referente às funcionalidades da Gameplay do jogo
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Gameplay where

import Card
import State
import StateManager

-- | Esta função recebe duas cartas (do jogador e do Bot, respectivamente) e
-- retorna um valor determinando o vencedor da rodada
getWinner :: Card -> Card -> Int
getWinner playerCard cpuCard
    | playerCard > cpuCard =  1 -- Vencedor: Jogador
    | playerCard < cpuCard = -1 -- Vencedor: Bot
    | otherwise            =  0 -- Empate

-- | Esta função incrementa o nível de faixa do jogador em +1
levelUpPlayer :: IO()
levelUpPlayer = do
    currentData <- getCampaignState
    let modifiedData = 
            CampaignState { 
                totalScore = totalScore currentData,                        
                lifes = lifes currentData,                        
                beltLevel = beltLevel currentData + 1 
            }
    
    writeCampaignState modifiedData

-- | Esta função recebe um valor a ser somado ao número total de 
-- vidas (Life Points) do jogador
updatePlayerLife :: Int -> IO()
updatePlayerLife lifeAmount = do
    currentData <- getCampaignState
    let modifiedData = 
            CampaignState {
                totalScore = totalScore currentData, 
                lifes = lifes currentData + lifeAmount, 
                beltLevel = beltLevel currentData
            }
    
    writeCampaignState modifiedData

-- | Esta função recebe a pontuação obtida pelo jogador na partida
-- e retorna soma com a pontuação atual da campanha
udpatePlayerCampaignScore :: Int -> IO()
udpatePlayerCampaignScore points = do
    currentData <- getCampaignState
    let modifiedData = 
            CampaignState { 
                totalScore = totalScore currentData + points, 
                lifes = lifes currentData, 
                beltLevel = beltLevel currentData
            }
            
    writeCampaignState modifiedData

-- | Esta função recebe um valor inteiro indicando o vencedor da rodada 
-- (1 indica que o jogador venceu e -1 indica que o bot venceu), a carta vencedora
-- e retorna a soma da pontuação atual com o poder da carta
updateScoreOf :: Int -> Card -> IO()
updateScoreOf winner card = do
    -- TODO utilização: ao executar (no main?), chamar com a função getWinner,
    --      definida mais acima no módulo.

    let modifiedPlayerScore = if winner == 1 then power card else 0
    let modifiedCPUScore = if winner == -1 then power card else 0
       
    currentData <- getBattleState

    let modifiedPlayerStreak = if winner == 1 then 1 + playerStreak currentData else 0
    let modifiedCPUStreak = if winner == -1 then 1 + cpuStreak currentData else 0

    let playerElementWin = if winner == 1 then modifyElemWinArray card (playerWinsByElement currentData) 
                           else playerWinsByElement currentData
    let cpuElementWin = if winner == -1 then modifyElemWinArray card (cpuWinsByElement currentData)
                        else cpuWinsByElement currentData

    let modifiedData = 
            BattleState { 
                currentRound = currentRound currentData,

                playerScore = playerScore currentData + modifiedPlayerScore,
                playerStreak = modifiedPlayerStreak, 
                playerWinsByElement = playerElementWin,
                playerDeck = playerDeck currentData,

                cpuScore = cpuScore currentData + modifiedCPUScore,
                cpuStreak = modifiedCPUStreak, 
                cpuWinsByElement = cpuElementWin,
                cpuDeck = cpuDeck currentData
            }
                          
    writeBattleState modifiedData

-- | Esta função verifica com os dados atuais do Battle State se algum lutador ganhou,
-- com o valor 1 definindo a vitória do jogador, valor -1 definindo a vitória do bot e
-- com o valor 0 dando a ideia de indefinição da vitória.
verifyVictory :: BattleState -> Int
verifyVictory battleState 
    | playerStreak battleState == 3 = 1
    | playerWinsByElement battleState == [True, True, True, True, True] = 1
    | cpuStreak battleState == 3 = -1
    | cpuWinsByElement battleState == [True, True, True, True, True] = -1
    | currentRound battleState == 11 && playerScore battleState > cpuScore battleState = 1
    | currentRound battleState == 11 && playerScore battleState < cpuScore battleState = -1
    | otherwise = 0

-- | Esta função recebe uma Carta e um array de booleanos que significa as vitórias
-- de elementos na luta em questão e modifica esse array, atualizando esses valores
-- booleanos para o estado correto.
modifyElemWinArray :: Card -> [Bool] -> [Bool]
modifyElemWinArray card winArray
    | element card == "fire" = replaceAtIndex winArray True 0
    | element card == "water" = replaceAtIndex winArray True 1
    | element card == "nature" = replaceAtIndex winArray True 2
    | element card == "metal" = replaceAtIndex winArray True 3
    | element card == "earth" = replaceAtIndex winArray True 4

-- | Função que recebe um array de um certo tipo e modifica o valor do índice dado
-- por um inteiro, pelo valor dado na chamada logo após o array original, retornando
-- essa cópia modificada do array original.
replaceAtIndex :: [a] -> a -> Int -> [a]
replaceAtIndex [] _ _ = []
replaceAtIndex (h:t) item indx
    | indx == 0 = item : t
    | indx <= 0 = (h:t)
    | otherwise = h : replaceAtIndex t item (indx - 1)