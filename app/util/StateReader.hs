{-
    Módulo referente à manipulação de estados do jogo
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StateReader where

import Data.Aeson
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as B
import StateData

-- | Esta função lê o arquivo de estado global e retorna seu conteúdo
-- ou um estado padrão (caso o arquivo seja inacessível)
getGlobalState :: GlobalState
getGlobalState = do
    let file = unsafePerformIO ( B.readFile (head toPath))
    let decodeData = decode file :: Maybe GlobalState
    case decodeData of
        Nothing -> GlobalState ""
        Just out -> out

-- | Esta função lê o arquivo de estado da campanha e retorna seu conteúdo
-- ou um estado padrão (caso o arquivo seja inacessível)
getCampaignState :: CampaignState
getCampaignState = do
    let file = unsafePerformIO ( B.readFile (toPath !! 1))
    let decodeData = decode file :: Maybe CampaignState
    case decodeData of
        Nothing -> CampaignState (-7) (-7) (-7)
        Just out -> out

-- | Esta função lê o arquivo de estado da partida e retorna seu conteúdo
-- ou um estado padrão (caso o arquivo seja inacessível)
getBattleState :: BattleState
getBattleState = do
    let file = unsafePerformIO ( B.readFile (toPath !! 2))
    let decodeData = decode file :: Maybe BattleState
    case decodeData of
        Nothing -> BattleState (-1) (-7) (-7) [] [] (-7) (-7) [] []
        Just out -> out
