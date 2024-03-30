{-
    Módulo referente às funcionalidades da Gameplay do jogo
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Gameplay where

import Card
import State
import StateManager
import Helpers

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
                playerName = playerName currentData,
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
                playerName = playerName currentData,
                totalScore = totalScore currentData,
                lifes = lifes currentData + lifeAmount,
                beltLevel = beltLevel currentData
            }

    writeCampaignState modifiedData

-- | Esta função recebe a pontuação obtida pelo jogador na partida
-- e retorna soma com a pontuação atual da campanha
updatePlayerCampaignScore :: Int -> IO()
updatePlayerCampaignScore points = do
    currentData <- getCampaignState
    let modifiedData =
            CampaignState {
                playerName = playerName currentData,
                totalScore = totalScore currentData + points,
                lifes = lifes currentData,
                beltLevel = beltLevel currentData
            }

    writeCampaignState modifiedData

-- | Esta função recebe nome do jogador no início da campanha
-- e o registra adequadamente.
updatePlayerName :: String -> IO()
updatePlayerName name = do
    currentData <- getCampaignState
    let modifiedData = 
            CampaignState {
                    playerName = name,
                    totalScore = totalScore currentData ,
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
                currentRound = currentRound currentData + 1,

                playerScore = playerScore currentData + modifiedPlayerScore,
                playerStreak = modifiedPlayerStreak,
                playerWinsByElement = playerElementWin,
                playerDeck = playerDeck currentData,

                cpuScore = cpuScore currentData + modifiedCPUScore,
                cpuStreak = modifiedCPUStreak,
                cpuWinsByElement = cpuElementWin,
                cpuDeck = cpuDeck currentData,

                specialDeck = specialDeck currentData,
                specialCardInUse = False
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

-- | Esta função reseta os dados do globals.json para os seus respectivos valores padrão
-- do início da partida. Mantém os rankings previamente armazenados.
resetGlobalState :: IO()
resetGlobalState = do
    previousGlobalState <- getGlobalState

    let modifiedData = GlobalState {
                screen = "menu",
                rankings = rankings previousGlobalState
            }

    writeGlobalState modifiedData

-- | Esta função reseta os dados do campaign.json para os seus respectivos valores padrão
-- do início de uma campanha.
resetCampaignState :: IO()
resetCampaignState = do
    let modifiedData = CampaignState {
                playerName = "",
                totalScore = 0,
                lifes = 2,
                beltLevel = 0
            }

    writeCampaignState modifiedData

-- | Esta função reseta os dados do battle.json para os seus respectivos valores padrão 
-- do início de uma batalha.
resetBattleState :: IO()
resetBattleState = do
    currentData <- getBattleState

    let modifiedData = BattleState {
                currentRound = 1,

                playerScore = 0,
                playerStreak = 0,
                playerWinsByElement = [False, False, False, False, False],
                playerDeck = playerDeck currentData,

                cpuScore = 0,
                cpuStreak = 0,
                cpuWinsByElement = [False, False, False, False, False],
                cpuDeck = cpuDeck currentData,

                specialDeck = ["swapInDeck", "nullifyElement", "swapBetweenDecks"],
                specialCardInUse = False
            }

    writeBattleState modifiedData

-- | Esta função recebe uma Carta e um array de booleanos que significa as vitórias
-- de elementos na luta em questão e modifica esse array, atualizando esses valores
-- booleanos para o estado correto.
modifyElemWinArray :: Card -> [Bool] -> [Bool]
modifyElemWinArray card winArray
    | element card == "fire" = replaceAtIndex winArray True 0
    | element card == "nature" = replaceAtIndex winArray True 1
    | element card == "water" = replaceAtIndex winArray True 2
    | element card == "metal" = replaceAtIndex winArray True 3
    | element card == "earth" = replaceAtIndex winArray True 4

-- | Função que verifica se há o uso de cartas especiais disponíveis e retorna uma dentre 3 
-- possibilidades de int, em que, 1 significa que a carta especial não foi utilizada ainda, 
-- 0 significa que a carta já está em uso e -1 denota que a carta especial já foi usada anteriormente.
verifySpecialCardAvailability :: BattleState -> Int
verifySpecialCardAvailability battleState
    | specialCardInUse battleState == False && lengthOfSpecialDeck battleState == 3 = 1
    | specialCardInUse battleState == True = 0
    | specialCardInUse battleState == False && lengthOfSpecialDeck battleState == 2 = -1

-- | Função que define o uso da carta especial, dado um inteiro que representa a carta
-- especial usada, fazendo as modificações necessárias no battleState para definir que
-- a carta dada está em uso.
useSpecialCard :: Int -> IO()
useSpecialCard index = do
    currentData <- getBattleState

    let newSpecialDeck = modifySpecialCardArray currentData index

    if index == 6 then swapInOwnDeck
    else if index == 8 then swapHandsCards else return ()

    let modifiedData =
            BattleState {
                currentRound = currentRound currentData,

                playerScore = playerScore currentData,
                playerStreak = playerStreak currentData,
                playerWinsByElement = playerWinsByElement currentData,
                playerDeck = playerDeck currentData,

                cpuScore = cpuScore currentData,
                cpuStreak = cpuStreak currentData,
                cpuWinsByElement = cpuWinsByElement currentData,
                cpuDeck = cpuDeck currentData,

                specialDeck = newSpecialDeck,
                specialCardInUse = True
            }

    writeBattleState modifiedData

-- | Função que recebe o Battle State atual e retorna um inteiro que verifica se uma 
-- carta especial está em uso, com o retorno sendo True se a carta de anulação está
-- sendo usada e False se não estiver sendo usada (em teoria, vai ser usado depois do jogador jogar a carta)
verifyNullifyElemSpecialCardUse :: BattleState -> Bool
verifyNullifyElemSpecialCardUse battleState
    | notElem "nullifyElement" (specialDeck battleState) && specialCardInUse battleState == True = True
    | otherwise = False

-- | Função que representa a primeira carta especial em que uma carta da mão do jogador é
-- trocada aleatoriamente com uma carta selecionada de maneira randômica também do deck do jogador.
swapInOwnDeck :: IO()
swapInOwnDeck = do
    currentData <- getBattleState

    let randomHandIndex = generateRandom 0 4
    let randomDeckIndex = generateRandom 5 (15 - (currentRound currentData))

    let card1 = playerDeck currentData !! randomHandIndex

    let card2 = playerDeck currentData !! randomDeckIndex

    let modifiedDeck = replaceAtIndex (playerDeck currentData) card1 randomDeckIndex
    let finalDeck = replaceAtIndex (modifiedDeck) card2 randomHandIndex

    let modifiedData = 
            BattleState {
                currentRound = currentRound currentData,

                playerScore = playerScore currentData,
                playerStreak = playerStreak currentData,
                playerWinsByElement = playerWinsByElement currentData,
                playerDeck = finalDeck,

                cpuScore = cpuScore currentData,
                cpuStreak = cpuStreak currentData,
                cpuWinsByElement = cpuWinsByElement currentData,
                cpuDeck = cpuDeck currentData,

                specialDeck = specialDeck currentData,
                specialCardInUse = specialCardInUse currentData
            }

    writeBattleState modifiedData

-- | Esta função determina a carta especial de anulação de elementos, recebendo duas cartas 
-- (do jogador e do Bot, respectivamente) e retorna um valor determinando o vencedor da rodada
getWinnerNullElement :: Card -> Card -> Int
getWinnerNullElement playerCard cpuCard
    | power playerCard > power cpuCard =  1 -- Vencedor: Jogador
    | power playerCard < power cpuCard = -1 -- Vencedor: Bot
    | otherwise            =  0 -- Empate

-- | Função que representa a carta especial de troca de cartas entre as mãos do jogador e bot, em que
-- uma carta da mão do jogador é trocada aleatoriamente com uma carta selecionada de maneira randômica
-- da mão do bot.
swapHandsCards :: IO()
swapHandsCards = do
    currentData <- getBattleState

    let randomPlayerHandIndex = generateRandom 0 4
    let randomCPUHandIndex = generateRandom 0 4

    let playerCard = playerDeck currentData !! randomPlayerHandIndex

    let cpuCard = cpuDeck currentData !! randomCPUHandIndex

    let modifiedPlayerDeck = replaceAtIndex (playerDeck currentData) cpuCard randomPlayerHandIndex
    let modifiedCPUDeck = replaceAtIndex (cpuDeck currentData) playerCard randomCPUHandIndex

    let modifiedData = 
            BattleState {
                currentRound = currentRound currentData,

                playerScore = playerScore currentData,
                playerStreak = playerStreak currentData,
                playerWinsByElement = playerWinsByElement currentData,
                playerDeck = modifiedPlayerDeck,

                cpuScore = cpuScore currentData,
                cpuStreak = cpuStreak currentData,
                cpuWinsByElement = cpuWinsByElement currentData,
                cpuDeck = modifiedCPUDeck,

                specialDeck = specialDeck currentData,
                specialCardInUse = specialCardInUse currentData
            }

    writeBattleState modifiedData

-- | Função que pega o Battle State e retorna o tamanho da lista de
-- cartas especiais.
lengthOfSpecialDeck :: BattleState -> Int
lengthOfSpecialDeck = length . specialDeck
    
-- | Função que recebe o battleState atual e um índice que representa a carta especial
-- que vai ser utilizada e retorna uma cópia dessa lista de cartas especiais com a 
-- remoção da escolhida para uso.
modifySpecialCardArray :: BattleState -> Int -> [String]
modifySpecialCardArray battleState index
    | index == 6 = removeElementList (specialDeck battleState) "swapInDeck"
    | index == 7 = removeElementList (specialDeck battleState) "nullifyElement"
    | index == 8 = removeElementList (specialDeck battleState) "swapBetweenDecks"
    | otherwise = specialDeck battleState