{-# LANGUAGE DeriveGeneric #-}

module State where

import Data.Aeson
import GHC.Generics
import Card
import Ranking

-- | Este tipo representa o estado global do jogo, incluindo
-- a tela atual e o ranking das seis melhores campanhas em ordem decrescente
data GlobalState = GlobalState {
    screen  :: String,
    rankings :: [Ranking]
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

-- Define que os estados serão tratam de arquivos JSON
instance FromJSON GlobalState
instance ToJSON GlobalState
instance FromJSON BattleState
instance ToJSON BattleState
instance FromJSON CampaignState
instance ToJSON CampaignState

-- | Essa função retorna os caminhos para os arquivos de estado do jogo
toPath :: [FilePath]
toPath = [
        "app/data/states/globals.json",
        "app/data/states/campaign.json",
        "app/data/states/battle.json"
    ]