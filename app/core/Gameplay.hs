{-
    Módulo referente às funcionalidades da Gameplay do jogo
-}

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

-- | Esta função recebe um valor inteiro indicando o vencedor da rodada, 
-- a carta vencedora e retorna a soma da pontuação atual com o poder da carta
updateScoreOf :: Int -> Card -> IO()
updateScoreOf winner card = do
    -- TODO utilização: ao executar (no main?), chamar com a função getWinner,
    --      definida mais acima no módulo.

    let modifiedPlayerScore = if winner == 1 then power card else 0
    let modifiedCPUScore = if winner == -1 then power card else 0
    
    let modifiedPlayerWins = if winner == 1 then 1 else 0
    let modifiedCPUWins = if winner == -1 then 1 else 0
    
    currentData <- getBattleState
    let modifiedData = 
            BattleState { 
                currentRound = currentRound currentData,

                playerScore = playerScore currentData + modifiedPlayerScore,
                playerWins = playerWins currentData + modifiedPlayerWins, 
                playerWinsByElement = playerWinsByElement currentData,
                playerDeck = playerDeck currentData,

                cpuScore = cpuScore currentData + modifiedCPUScore,
                cpuWins = cpuWins currentData + modifiedCPUWins, 
                cpuWinsByElement = cpuWinsByElement currentData,
                cpuDeck = cpuDeck currentData
            }
                          
    writeBattleState modifiedData