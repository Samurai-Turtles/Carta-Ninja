{-# LANGUAGE OverloadedStrings #-}

module StateWriter where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import State
import StateReader
import System.Directory
import System.FilePath.Posix (takeDirectory)
import Card

writeBattleRound :: Int -> IO ()
writeBattleRound newCurRound = do
    let newBattleState = BattleState newCurRound (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattlePScore :: Int -> IO ()
writeBattlePScore newCurScore = do
    let newBattleState = BattleState (currentRound getBattleState) newCurScore (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattlePWins :: Int -> IO ()
writeBattlePWins newCurWins = do
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) newCurWins (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattlePWinFire :: Bool -> IO ()
writeBattlePWinFire bool = do
    let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 0
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattlePWinWater :: Bool -> IO ()
writeBattlePWinWater bool = do
    let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 1
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)


writeBattlePWinNature :: Bool -> IO ()
writeBattlePWinNature bool = do
    let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 2
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattlePWinMetal :: Bool -> IO ()
writeBattlePWinMetal bool = do
    let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 3
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattlePWinEarth :: Bool -> IO ()
writeBattlePWinEarth bool = do
    let newWinArray = replaceAtIndex (playerWinsByElement getBattleState) bool 4
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) newWinArray 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattlePDeck :: [Card] -> IO ()
writeBattlePDeck newDeck = do
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         newDeck (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattleCScore :: Int -> IO ()
writeBattleCScore newCurScore = do
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) newCurScore (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattleCWins :: Int -> IO ()
writeBattleCWins newCurWins = do
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) newCurWins (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattleCWinFire :: Bool -> IO ()
writeBattleCWinFire bool = do
    let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 0
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattleCWinWater :: Bool -> IO ()
writeBattleCWinWater bool = do
    let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 1
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState)
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)


writeBattleCWinNature :: Bool -> IO ()
writeBattleCWinNature bool = do
    let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 2
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattleCWinMetal :: Bool -> IO ()
writeBattleCWinMetal bool = do
    let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 3
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattleCWinEarth :: Bool -> IO ()
writeBattleCWinEarth bool = do
    let newWinArray = replaceAtIndex (cpuWinsByElement getBattleState) bool 4
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) newWinArray (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeBattleCDeck :: [Card] -> IO ()
writeBattleCDeck newDeck = do
    let newBattleState = BattleState (currentRound getBattleState) (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                         (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) newDeck

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newBattleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)

writeGlobalsScreen :: String -> IO ()
writeGlobalsScreen newScreen = do
    let newGlobalState = GlobalState newScreen

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newGlobalState)
    removeFile (toPath !! 0)
    renameFile "../core/states/Temp.json" (toPath !! 0)

writeCampaignScore :: Int -> IO ()
writeCampaignScore newScore = do
    let newCampaignState = CampaignState newScore (lifes getCampaignState) (beltLevel getCampaignState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newCampaignState)
    removeFile (toPath !! 1)
    renameFile "../core/states/Temp.json" (toPath !! 1)

writeCampaignLifes :: Int -> IO ()
writeCampaignLifes newLifes = do
    let newCampaignState = CampaignState (totalScore getCampaignState) newLifes (beltLevel getCampaignState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newCampaignState)
    removeFile (toPath !! 1)
    renameFile "../core/states/Temp.json" (toPath !! 1)

writeCampaignBeltLvl :: Int -> IO ()
writeCampaignBeltLvl newBeltLvl = do
    let newCampaignState = CampaignState (totalScore getCampaignState) (lifes getCampaignState) newBeltLvl

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode newCampaignState)
    removeFile (toPath !! 1)
    renameFile "../core/states/Temp.json" (toPath !! 1)

createFile :: FilePath -> IO ()
createFile path = do
    createDirectoryIfMissing True (takeDirectory path)

replaceAtIndex :: [a] -> a -> Int -> [a]
replaceAtIndex [] _ _ = []
replaceAtIndex (h:t) item indx
    | indx == 0 = item : t
    | indx <= 0 = (h:t)
    | otherwise = h : replaceAtIndex t item (indx - 1)