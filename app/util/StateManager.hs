{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

module StateManager where

import Data.Aeson
import GHC.Generics
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as B

-- | Este tipo representa o estado global do jogo, incluindo
-- a tela atual e o ranking das seis melhores campanhas em ordem decrescente
data GeneralState = GeneralState {
    screen :: String,
    ranking :: [String]
} deriving (Generic, Show)

-- | Este tipo representa o estado da campanha atual, incluindo
-- o total de pontos da campanha, o número de vidas e o nível atual
data CampaignState = CampaignState {
    pontosGerais :: Int,
    vidas :: Int,
    nivel :: Int
} deriving (Generic, Show)

-- | Este tipo representa o estado da partida atual, incluindo
-- a pountação, vitórias e Deck de cada jogador, bem como a rodada atual
data GameplayState = GameplayState {
    player_score :: Int,
    player_vitorias :: Int,
    player_vitoriasElementos :: [Bool],
    player_deck :: [String],

    cpu_score :: Int,
    cpu_vitorias :: Int,
    cpu_vitoriasElementos :: [Bool],
    cpu_deck :: [String],

    rodada :: Int
} deriving (Generic, Show)

-- Define que os estados serão extraídos de arquivos JSON
instance FromJSON GeneralState
instance FromJSON GameplayState
instance FromJSON CampaignState

-- Estas funções geram estados de fallback caso os arquivos sejam inacessíveis
fallbackGeneral :: GeneralState
fallbackGeneral = GeneralState "" []

fallbackCampaign :: CampaignState
fallbackCampaign = CampaignState (-7) (-7) (-7)

fallbackGameplay :: GameplayState
fallbackGameplay = GameplayState (-7) (-7) [] [] (-7) (-7) [] [] (-1)

-- | Essa função retorna os caminhos para os arquivos de estado do jogo
toPath :: [FilePath]
toPath = [
        "app/core/states/globals.json", 
        "app/core/states/campaign.json", 
        "app/core/states/battle.json"
    ]

-- | Esta função lê o arquivo de estado global
getGeneralData :: GeneralState
getGeneralData = do
    let file = unsafePerformIO( B.readFile (toPath !! 0))
    let decodeData = decode file :: Maybe (GeneralState)
    case decodeData of
        Nothing -> fallbackGeneral
        Just out -> out

-- | Esta função lê o arquivo de estado da campanha
getCampaignData :: CampaignState
getCampaignData = do
    let file = unsafePerformIO( B.readFile (toPath !! 1))
    let decodeData = decode file :: Maybe (CampaignState) 
    case decodeData of
        Nothing -> fallbackCampaign
        Just out -> out

-- | Esta função lê o arquivo de estado da partida
getGameplayData :: GameplayState
getGameplayData = do
    let file = unsafePerformIO( B.readFile (toPath !! 2))
    let decodeData = decode file :: Maybe (GameplayState)
    case decodeData of
        Nothing -> fallbackGameplay
        Just out -> out
