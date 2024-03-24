{-
    Módulo referente às funcionalidades da Gameplay do jogo
-}

module Gameplay where

import Card
import StateManager

-- | Esta função recebe duas cartas (do jogador e do Bot, respectivamente) e
-- retorna um valor determinando o vencedor da rodada
getWinner :: Card -> Card -> Int
getWinner playerCard cpuCard
    | playerCard > cpuCard =  1 -- Vencedor: Jogador
    | playerCard < cpuCard = -1 -- Vencedor: Bot
    | otherwise            =  0 -- Empate

-- | Esta função incrementa o nível de faixa do jogador em +1
levelUpPlayer :: Int
levelUpPlayer = nivel getCampaignData + 1

-- | Esta função recebe um valor a ser somado ao número total de 
-- vidas (Life Points) do jogador
updatePlayerLife :: Int -> Int
updatePlayerLife value = vidas getCampaignData + value

-- | Esta função recebe a pontuação obtida pelo jogador na partida
-- e retorna soma com a pontuação atual da campanha
updatePlayerCP :: Int -> Int
updatePlayerCP points = pontosGerais getCampaignData + points

-- | Esta função recebe um caractere indicando o vencedor da rodada, 
-- a carta vencedora e retorna a soma da pontuação atual com o poder da carta
updateScoreOf :: Char -> Card -> Int
updateScoreOf player card = power card + currentScore
    where 
        currentScore = if player == 'p' 
            then player_score getGameplayData 
            else cpu_score getGameplayData
