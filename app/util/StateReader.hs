{-
    Módulo referente à manipulação de estados do jogo
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StateReader where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import State

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
    
