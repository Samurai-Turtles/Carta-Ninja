module Tip where

import Card
import Bot
import System.Random

-- mão do jogador -> deck a IA
giveTip :: BattleState -> String
giveTip battlestate 
    | score <= 20 = tipBotWillPlay battlestate
    | score <= 40 = tipToPlay battlestate
    | otherwise   = tipNotToPlay battlestate
    where score = playerScore battlestate

tipBotWillPlay :: BattleState -> String
tipBotWillPlay battlestate = "o bot talvez jogue: " ++ tipBotAux battlestate

tipBotAux :: BattleState -> String
tipBotAux battlestate = do
    let botDeck = cpuDeck battlestate
    elemento (botDeck !! makeChoice battlestate 10)


tipToPlay :: BattleState -> String
tipToPlay battlestate = do
    let seed = cpuScore battlestate
    let weight = wDeck (cpuDeck battlestate) 10
    let player_hand = take 5 (playerDeck battlestate)
    let w_player_hand = wHand player_hand weight
    let len = length w_player_hand

    "você deveria jogar: " ++ elemento (player_hand !! (w_player_hand !! nRandom seed (len - 1)))

tipNotToPlay :: BattleState -> String
tipNotToPlay battlestate = do 
    let botplay = tipBotAux
    case botplay of
        "fire" -> tipNotAux "nature"
        "nature" -> tipNotAux "water"
        "water" -> tipNotAux "metal"
        "metal" -> tipNotAux "earth"
        "earth" -> tipNotAux "fire"

tipNotAux :: String -> String
tipNotAux elem = "você não deveria jogar: " ++ elem