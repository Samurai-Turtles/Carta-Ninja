{-# LANGUAGE OverloadedStrings #-}

module StateWriter where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import State
import StateReader
import System.Directory
import System.FilePath.Posix (takeDirectory)
import Card
import GHC.Num.BigNat (BigNat)

-- TODO reduzir a quantidade de funções.
-- TODO adaptar as funções à nova estratégia de trabalhar com JSONs.

-- writeBattleRound :: Int -> IO ()
-- writeBattleRound newCurRound = do
--     let newBattleState = BattleState newCurRound (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattlePScore :: Int -> IO ()
-- writeBattlePScore newCurScore = do
--     let newBattleState = BattleState (currentRound getBattleState) newCurScore (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattlePWins :: Int -> IO ()
-- writeBattlePWins newCurWins = do
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) newCurWins (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattlePWinFire :: Bool -> IO ()
-- writeBattlePWinFire bool = do
--     let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 0
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattlePWinWater :: Bool -> IO ()
-- writeBattlePWinWater bool = do
--     let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 1
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattlePWinNature :: Bool -> IO ()
-- writeBattlePWinNature bool = do
--     let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 2
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattlePWinMetal :: Bool -> IO ()
-- writeBattlePWinMetal bool = do
--     let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 3
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattlePWinEarth :: Bool -> IO ()
-- writeBattlePWinEarth bool = do
--     let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 4
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattlePDeck :: [Card] -> IO ()
-- writeBattlePDeck newDeck = do
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          newDeck (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattleCScore :: Int -> IO ()
-- writeBattleCScore newCurScore = do
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) newCurScore (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattleCWins :: Int -> IO ()
-- writeBattleCWins newCurWins = do
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) newCurWins (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattleCWinFire :: Bool -> IO ()
-- writeBattleCWinFire bool = do
--     let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 0
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattleCWinWater :: Bool -> IO ()
-- writeBattleCWinWater bool = do
--     let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 1
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattleCWinNature :: Bool -> IO ()
-- writeBattleCWinNature bool = do
--     let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 2
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattleCWinMetal :: Bool -> IO ()
-- writeBattleCWinMetal bool = do
--     let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 3
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattleCWinEarth :: Bool -> IO ()
-- writeBattleCWinEarth bool = do
--     let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 4
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

--     writeOnJsonBattle newBattleState

-- writeBattleCDeck :: [Card] -> IO ()
-- writeBattleCDeck newDeck = do
--     let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
--                          (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) newDeck

--     writeOnJsonBattle newBattleState

-- writeCampaignScore :: Int -> IO ()
-- writeCampaignScore newScore = do
--     let newCampaignState = CampaignState newScore (lifes getCampaignState) (beltLevel getCampaignState)

--     writeOnJsonCampaign newCampaignState

-- writeCampaignLifes :: Int -> IO ()
-- writeCampaignLifes newLifes = do
--     let newCampaignState = CampaignState (totalScore getCampaignState) newLifes (beltLevel getCampaignState)

--     writeOnJsonCampaign newCampaignState

-- writeCampaignBeltLvl :: Int -> IO ()
-- writeCampaignBeltLvl newBeltLvl = do
--     let newCampaignState = CampaignState (totalScore getCampaignState) (lifes getCampaignState) newBeltLvl

--     writeOnJsonCampaign newCampaignState

writeGlobalsScreen :: String -> IO ()
writeGlobalsScreen newScreen = do
    let newGlobalState = GlobalState newScreen

    writeOnJsonGlobals newGlobalState

writeOnJsonBattle :: BattleState -> IO ()
writeOnJsonBattle newBattleState = do
    B.writeFile (toPath !! 2) (encode newBattleState)

writeOnJsonCampaign :: CampaignState -> IO ()
writeOnJsonCampaign newCampaignState = do
    B.writeFile (toPath !! 1) (encode newCampaignState)

writeOnJsonGlobals :: GlobalState -> IO ()
writeOnJsonGlobals newGlobalState = do
    B.writeFile (head toPath) (encode newGlobalState)

createFile :: FilePath -> IO ()
createFile path = do
    createDirectoryIfMissing True (takeDirectory path)

replaceAtIndex :: [a] -> a -> Int -> [a]
replaceAtIndex [] _ _ = []
replaceAtIndex (h:t) item indx
    | indx == 0 = item : t
    | indx <= 0 = (h:t)
    | otherwise = h : replaceAtIndex t item (indx - 1)