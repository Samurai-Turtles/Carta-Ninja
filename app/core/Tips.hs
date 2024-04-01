module Tips (giveTip) where

import Card
import Bot
import State


-- mão do jogador -> deck a IA
giveTip :: BattleState -> String
giveTip battlestate 
    | score <= 20 = tipBotWillPlay battlestate
    | score <= 40 = tipToPlay battlestate
    | otherwise   = tipNotToPlay battlestate
    where score = playerScore battlestate

tipBotWillPlay :: BattleState -> String
tipBotWillPlay battlestate = "O bot talvez jogue: " ++ tipBotAux battlestate

tipBotAux :: BattleState -> String
tipBotAux battlestate = do
    let botDeck = cpuDeck battlestate
    auxTranslate $ element (botDeck !! makeChoice battlestate 10)


tipToPlay :: BattleState -> String
tipToPlay battlestate = do
    let seed = cpuScore battlestate
    let weight = wDeck (cpuDeck battlestate) 10
    let player_hand = take 5 (playerDeck battlestate)
    let w_player_hand = wHand player_hand weight
    let len = length w_player_hand

    "Você deveria jogar: " ++ auxTranslate (element (player_hand !! (w_player_hand !! nRandom seed (len - 1))))

tipNotToPlay :: BattleState -> String
tipNotToPlay battlestate = do 
    let botplay = tipBotAux battlestate
    case botplay of
        "Fogo" -> tipNotAux "Natureza"
        "Natureza" -> tipNotAux "Água"
        "Água" -> tipNotAux "Metal"
        "Metal" -> tipNotAux "Terra"
        "Terra" -> tipNotAux "Fogo"
        _ -> "Fogo"
    
tipNotAux :: String -> String
tipNotAux tipo = "Você não deveria jogar: " ++ tipo

-- | Retorna o tipo de elemento correspondente ao tipo definido na carta.
auxTranslate :: String -> String
auxTranslate word
    | word == "fire" = "Fogo"
    | word == "nature" = "Natureza"
    | word == "water" = "Água"
    | word == "metal" = "Metal"
    | otherwise = "Terra"