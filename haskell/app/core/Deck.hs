{-
    Módulo referente às funcionalidades relacionadas ao Deck de cartas do jogo.
 -}

module Deck (
    playCard
) where

import Card
import Helpers

-- | Esta função recebe o índice da carta jogada e retorna o Deck
-- após a jogada ser feita (com a carta jogada no final)
playCard :: [Card] -> Int -> [Card]
playCard cards index =
    if validateIndex index (0, 4)
        then pushElementToEnd index cards
        else []