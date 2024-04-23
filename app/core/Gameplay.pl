:- consult('../models/Card.pl').

/* Predicado que verifica qual carta dada ganhou a rodada a partir de uma comparação
 de seus elementos, retornando 1 para os casos que o jogador ganha, -1 quando o bot ganha
 e 0 para quando há empate.
 */
getWinner(card(id(IdP), elem(ElemP), power(PowerP)), card(id(IdC), elem(ElemC), power(PowerC)), R) :-
    (ElemP = ElemC),
    getWinnerByPower(card(id(IdP), elem(ElemP), power(PowerP)), card(id(IdC), elem(ElemC), power(PowerC)), O),
    R is O,
    !.
getWinner(card(id(), elem("fire"), power()), card(id(), elem(ElemC), power()), R) :-
    (ElemC = "nature" ; ElemC = "metal"),
    R is 1,
    !.
getWinner(card(id(), elem("metal"), power()), card(id(), elem(ElemC), power()), R) :-
    (ElemC = "nature" ; ElemC = "earth"),
    R is 1,
    !.
getWinner(card(id(), elem("nature"), power()), card(id(), elem(ElemC), power()), R) :-
    (ElemC = "earth" ; ElemC = "water"),
    R is 1,
    !.
getWinner(card(id(), elem("earth"), power()), card(id(), elem(ElemC), power()), R) :-
    (ElemC = "fire" ; ElemC = "water"),
    R is 1,
    !.
getWinner(card(id(), elem("water"), power()), card(id(), elem(ElemC), power()), R) :-
    (ElemC = "fire" ; ElemC = "metal"),
    R is 1,
    !.
getWinner(card(id(), elem(), power()), card(id(), elem(), power()), -1).


/* Predicado que verifica qual carta dada ganhou a rodada a partir de uma comparação
 do poder de ambas, retornando 1 para o caso em que o jogador ganha, -1 quando o bot ganha
 e 0 para quando há empate.
 */
getWinnerByPower(card(id(), elem(), power(PowerP)), card(id(), elem(), power(PowerC)), R) :-
    (PowerP > PowerC),
    R is 1,
    !.
getWinnerByPower(card(id(), elem(), power(PowerP)), card(id(), elem(), power(PowerC)), R) :-
    (PowerP < PowerC),
    R is -1,
    !.
getWinnerByPower(card(id(), elem(), power()), card(id(), elem(), power()), 0).