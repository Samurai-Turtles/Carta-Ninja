{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module UI.TemplateData.LeitorData where

import Data.Aeson
import GHC.Generics
import System.IO
import System.IO.Unsafe
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

data GeneralState = GeneralState {
    screen :: String,
    ranking :: [String]
} deriving (Generic, Show)

data GameplayState = GameplayState {
    h_pontosGerais :: Int,
    h_vidas :: Int,
    h_faixa :: Int
} deriving (Generic, Show)

data CampaignState = CampaignState {
    h_pontuação :: Int,
    h_vitorias :: Int,
    h_vitoriasElementos :: [Bool],
    h_deck :: [String],
    rodada :: Int
} deriving (Generic, Show)

generalPath = "app/UI/TemplateData/GameState/GeneralState.json"
gameplayPath = "app/UI/TemplateData/GameState/GameplayState.json"
campaignState = "app/UI/TemplateData/GameState/CampaignState.json"

instance FromJSON GeneralState
instance FromJSON GameplayState
instance FromJSON CampaignState


getGeneralData :: GeneralState
getGeneralData = do
    let readFile = unsafePerformIO( B.readFile generalPath )
    let decodeData = decode readFile :: Maybe (GeneralState)
    case decodeData of
        Nothing -> GeneralState "" []
        Just out -> out

getGameplayData :: GameplayState
getGameplayData = do
    let readFile = unsafePerformIO( B.readFile gameplayPath )
    let decodeData = decode readFile :: Maybe (GameplayState)
    case decodeData of
        Nothing -> GameplayState (-7) (-7) (-7)
        Just out -> out

getCampaignData :: CampaignState 
getCampaignData = do
    let readFile = unsafePerformIO( B.readFile campaignState )
    let decodeData = decode readFile :: Maybe (CampaignState) 
    case decodeData of
        Nothing -> CampaignState (-7) (-7) [] [] (-7)
        Just out -> out