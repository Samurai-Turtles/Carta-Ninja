{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ReadData where

import Data.Aeson
import System.IO
import GHC.Generics
import System.IO.Unsafe
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC


data GeneralState = GeneralState {
    screen :: String,
    ranking :: [String]
} deriving (Generic, Show)

data CampaignState = CampaignState {
    pontosGerais :: Int,
    vidas :: Int,
    nivel :: Int
} deriving (Generic, Show)

data GameplayState = GameplayState {
    h_score :: Int,
    h_vitorias :: Int,
    h_vitoriasElementos :: [Bool],
    h_deck :: [String],

    c_score :: Int,
    c_vitorias :: Int,
    c_vitoriasElementos :: [Bool],
    c_deck :: [String],

    rodada :: Int
} deriving (Generic, Show)

instance FromJSON GeneralState
instance FromJSON GameplayState
instance FromJSON CampaignState

standardGeneral = GeneralState "" []
standardCampaign = CampaignState (-7) (-7) (-7)
standardGameplay = GameplayState (-7) (-7) [] [] (-7) (-7) [] [] (-1)

toPath = ["app/core/GameState/GeneralState.json", 
        "app/core/GameState/CampaignState.json", 
        "app/core/GameState/GameplayState.json"]

getGeneralData :: GeneralState
getGeneralData = do
    let readFile = unsafePerformIO( B.readFile (toPath !! 0))
    let decodeData = decode readFile :: Maybe (GeneralState)
    case decodeData of
        Nothing -> standardGeneral
        Just out -> out

getCampaignData :: CampaignState
getCampaignData = do
    let readFile = unsafePerformIO( B.readFile (toPath !! 1))
    let decodeData = decode readFile :: Maybe (CampaignState) 
    case decodeData of
        Nothing -> standardCampaign
        Just out -> out

getGameplayData :: GameplayState
getGameplayData = do
    let readFile = unsafePerformIO( B.readFile (toPath !! 2))
    let decodeData = decode readFile :: Maybe (GameplayState)
    case decodeData of
        Nothing -> standardGameplay
        Just out -> out
