/* Esse predicado verifica casos de exceção quanto à
 * carta jogada antes de realmente colocá-la no final
 * do deck
 */
pushCardToEnd(_, [], []):- !.
pushCardToEnd(_, [X], [X]):- !.
pushCardToEnd(Idx, Deck, ReturnedDeck):-
    length(Deck, SizeOfDeck),
    (Idx < 0 ; Idx >= SizeOfDeck),
    ReturnedDeck = Deck,
    !.
pushCardToEnd(Idx, Deck, ReturnedDeck):-
    elementToEnd(Idx, Deck, R),
    ReturnedDeck = R.

/* Predicado que dado um índice válido e uma lista pega o 
 * elemento desse índice e move para o final, retornando 
 * essa lista com essa modificação.
 */
elementToEnd(Idx, [H|T], ReturnedList):-
    (Idx =:= 0),
    append(T, [H], R),
    ReturnedList = R,
    !.
elementToEnd(Idx, [H|T], ReturnedList):-
    NewIdx is Idx - 1,
    elementToEnd(NewIdx, T, R1),
    append([H], R1, R),
    ReturnedList = R.

% Dado uma carta, retorne o elemento dela.
getElem(card(id(_), elem(Element), power(_)), Element).