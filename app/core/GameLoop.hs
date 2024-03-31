module GameLoop where


import Render (action)
import StateManager
import State (GlobalState(GlobalState, screen, rankings))

import System.Process
import System.Info (os)
import Data.Char (toUpper)
import Gameplay (resetCampaignState, updatePlayerName)

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
menuResolve "S" = if os == "mingw32" then callCommand "cls" else callCommand "clear"
menuResolve _ = menuLoop

-- | Esta executa o loop sobre as possibilidades de escolha na tela de ranking.
-- O loop é repetido até o jogador digitar um valor possível para escolha.
rankingLoop :: IO()
rankingLoop = do
    localUpdateScreen "ranking"
    action

    input <- getLine
    let choice = validInput ["V"] input
    if choice /= "" then menuLoop else rankingLoop

-- | Esta executa o loop sobre as possibilidades de escolha na tela de creditos.
-- O loop é repetido até o jogador digitar um valor possível para escolha.
creditLoop :: IO()
creditLoop = do
    localUpdateScreen "creditos"
    action

    input <- getLine
    let choice = validInput ["V"] input
    if choice /= "" then menuLoop else creditLoop


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
    
    battleLoop


battleLoop :: IO()
battleLoop = do
    input <- getLine

    let choice = validInput ["1", "2", "3", "4", "5", "6", "7", "8", "D"] input

    print ""

------------------------------ Funções Auxiliares ------------------------------

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