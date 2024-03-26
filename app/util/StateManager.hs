{-
    Módulo referente à leitura e escrita dos arquivos de estado JSON.
-}

module StateManager (
    getGlobalState,
    getCampaignState,
    getBattleState,
    writeGlobalState,
    writeCampaignState,
    writeBattleState
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import State

------------------------------ Funções de Leitura ------------------------------ 

-- | Esta função lê o arquivo de estado global e retorna seu conteúdo
-- ou um erro (caso o arquivo seja inacessível)
getGlobalState :: IO GlobalState
getGlobalState = do
    contents <- B.readFile (head toPath)
    case eitherDecode contents of
        Left err -> error err
        Right json -> return json

-- | Esta função lê o arquivo de estado da campanha e retorna seu conteúdo
-- ou um erro (caso o arquivo seja inacessível)
getCampaignState :: IO CampaignState
getCampaignState = do
    contents <- B.readFile (toPath !! 1)
    case eitherDecode contents of
        Left err -> error err
        Right json -> return json

-- | Esta função lê o arquivo de estado da partida e retorna seu conteúdo
-- ou um estado padrão (caso o arquivo seja inacessível)
getBattleState :: IO BattleState
getBattleState = do
    contents <- B.readFile (toPath !! 2)
    case eitherDecode contents of
        Left err -> error err
        Right json -> return json
    
------------------------------ Funções de Escrita ------------------------------

-- | Esta função recebe um novo `GlobalState` e o grava no 
-- arquivo JSON correspondente
writeGlobalState :: GlobalState -> IO ()
writeGlobalState newGlobalState = do
    B.writeFile (head toPath) (encode newGlobalState)

-- | Esta função recebe um novo `CampaignState` e o grava no 
-- arquivo JSON correspondente
writeCampaignState :: CampaignState -> IO ()
writeCampaignState newCampaignState = do
    B.writeFile (toPath !! 1) (encode newCampaignState)

-- | Esta função recebe um novo `BattleState` e o grava no 
-- arquivo JSON correspondente
writeBattleState :: BattleState -> IO ()
writeBattleState newBattleState = do
    B.writeFile (toPath !! 2) (encode newBattleState)
