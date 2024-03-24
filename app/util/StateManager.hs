{-
    Módulo referente à manipulação de estados do jogo
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StateManager where

import Data.Aeson
import GHC.Generics
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as B

import Card

-- | Este tipo representa o estado global do jogo, incluindo
-- a tela atual e o ranking das seis melhores campanhas em ordem decrescente
data GlobalState = GlobalState {
    screen  :: String,
    ranking :: [String]
} deriving (Generic, Show)

-- | Este tipo representa o estado da campanha atual, incluindo
-- o total de pontos da campanha, o número de vidas e o nível atual
data CampaignState = CampaignState {
    totalScore :: Int,
    lifes      :: Int,
    beltLevel  :: Int
} deriving (Generic, Show)

-- | Este tipo representa o estado da partida atual, incluindo
-- a pountação, vitórias e Deck de cada jogador, bem como a rodada atual.
data BattleState = BattleState {
    -- TODO: adicionar propriedade `streak` para jogador e bot
    currentRound        :: Int,

    playerScore         :: Int,      -- Pontuação do Jogador
    playerWins          :: Int,      -- Vitórias do Jogador
    playerWinsByElement :: [Bool],   -- Vitórias do Jogador por elemento
    playerDeck          :: [Card],   -- Deck de cartas do Jogador

    cpuScore            :: Int,      -- Pontuação do Bot
    cpuWins             :: Int,      -- Vitórias do Bot
    cpuWinsByElement    :: [Bool],   -- Vitórias do Bot por elemento
    cpuDeck             :: [Card]    -- Deck de cartas do Bot
} deriving (Generic, Show)

-- Define que os estados serão extraídos de arquivos JSON
instance FromJSON GlobalState
instance FromJSON BattleState
instance FromJSON CampaignState

-- | Essa função retorna os caminhos para os arquivos de estado do jogo
toPath :: [FilePath]
toPath = [
        "app/core/states/globals.json",
        "app/core/states/campaign.json",
        "app/core/states/battle.json"
    ]

-- | Esta função lê o arquivo de estado global e retorna seu conteúdo
-- ou um estado padrão (caso o arquivo seja inacessível)
getGlobalState :: GlobalState
getGlobalState = do
    let file = unsafePerformIO ( B.readFile (head toPath))
    let decodeData = decode file :: Maybe GlobalState
    case decodeData of
        Nothing -> GlobalState "" []
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
