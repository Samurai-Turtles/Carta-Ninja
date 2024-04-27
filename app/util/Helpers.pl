/* Esse predicado verifica casos de exceção quanto à
 * carta jogada antes de realmente colocá-la no final
 * do deck
 */
push_card_to_end(_, [], []):- !.
push_card_to_end(_, [X], [X]):- !.
push_card_to_end(Idx, Deck, ReturnedDeck):-
    length(Deck, SizeOfDeck),
    (Idx < 0 ; Idx >= SizeOfDeck),
    ReturnedDeck = Deck,
    !.
push_card_to_end(Idx, Deck, ReturnedDeck):-
    element_to_end(Idx, Deck, R),
    ReturnedDeck = R.

/* Predicado que dado um índice válido e uma lista pega o 
 * elemento desse índice e move para o final, retornando 
 * essa lista com essa modificação.
 */
element_to_end(Idx, [H|T], ReturnedList):-
    (Idx =:= 0),
    append(T, [H], R),
    ReturnedList = R,
    !.
element_to_end(Idx, [H|T], ReturnedList):-
    NewIdx is Idx - 1,
    element_to_end(NewIdx, T, R1),
    append([H], R1, R),
    ReturnedList = R.

% Dado uma carta, retorne o elemento dela.
get_elem(card(id(_), elem(Element), power(_)), Element).