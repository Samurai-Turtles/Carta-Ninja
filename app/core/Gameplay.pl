:- consult('../models/Card.pl').
:- consult('../util/Helpers.pl').

% Arquivo que trata das funcionalidades lógicas básicas de gameplay.

/* Predicado que verifica qual carta dada ganhou a rodada a 
 * partir de uma comparação de seus elementos, retornando 1 para os 
 * casos que o jogador ganha, -1 quando o bot ganha e 0 para quando 
 * há empate.
 */
getWinner(card(id(IdP), elem(ElemP), power(PowerP)), card(id(IdC), elem(ElemC), power(PowerC)), R):-
    (ElemP = ElemC),
    getWinnerByPower(card(id(IdP), elem(ElemP), power(PowerP)), card(id(IdC), elem(ElemC), power(PowerC)), O),
    R is O,
    !.
getWinner(card(id(_), elem("fire"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "nature" ; ElemC = "metal"),
    R is 1,
    !.
getWinner(card(id(_), elem("metal"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "nature" ; ElemC = "earth"),
    R is 1,
    !.
getWinner(card(id(_), elem("nature"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "earth" ; ElemC = "water"),
    R is 1,
    !.
getWinner(card(id(_), elem("earth"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "fire" ; ElemC = "water"),
    R is 1,
    !.
getWinner(card(id(_), elem("water"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "fire" ; ElemC = "metal"),
    R is 1,
    !.
getWinner(card(id(_), elem(_), power(_)), card(id(_), elem(_), power(_)), -1).


/* Predicado que verifica qual carta dada ganhou a rodada a partir de uma
 * comparação do poder de ambas, retornando 1 para o caso em que o jogador
 * ganha, -1 quando o bot ganha e 0 para quando há empate.
 */
getWinnerByPower(card(id(_), elem(_), power(PowerP)), card(id(_), elem(_), power(PowerC)), R):-
    (PowerP > PowerC),
    R is 1,
    !.
getWinnerByPower(card(id(_), elem(_), power(PowerP)), card(id(_), elem(_), power(PowerC)), R):-
    (PowerP < PowerC),
    R is -1,
    !.
getWinnerByPower(card(id(_), elem(_), power(_)), card(id(_), elem(_), power(_)), 0).

/* Predicado que receb o índice da carta jogada e o deck, retornando
 * o deck após a jogada ser feita, com a carta jogada no final do deck.
 */
playCard(_, Idx, NewDeck):-
    (Idx < 0 ; Idx > 4),
    NewDeck = [],
    !.
playCard(Deck, Idx, NewDeck):-
    pushCardToEnd(Idx, Deck, R),
    NewDeck = R.