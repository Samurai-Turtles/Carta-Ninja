{-# LANGUAGE OverloadedStrings #-}

module StateWriter where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import State
import StateReader
import System.Directory
import System.FilePath.Posix (takeDirectory)

createFile :: FilePath -> IO ()
createFile path = do
  createDirectoryIfMissing True (takeDirectory path)

writeGlobalsScreen :: String -> IO ()
writeGlobalsScreen newScreen = do
    let globalState = GlobalState newScreen

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode globalState)
    removeFile (toPath !! 0)
    renameFile "../core/states/Temp.json" (toPath !! 0)

writeBattleRound :: Int -> IO ()
writeBattleRound newCurRound = do
    let battleState = BattleState newCurRound (playerScore getBattleState) (playerWins getBattleState) (playerWinsByElement getBattleState) 
                      (playerDeck getBattleState) (cpuScore getBattleState) (cpuWins getBattleState) (cpuWinsByElement getBattleState) (cpuDeck getBattleState)

    createFile "../core/states/Temp.json"
    B.writeFile "../core/states/Temp.json" (encode battleState)
    removeFile (toPath !! 2)
    renameFile "../core/states/Temp.json" (toPath !! 2)