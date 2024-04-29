% Arquivo que trata de funcionalidades auxiliadoras gerais.

/*
 * Esse predicado verifica casos de exceção quanto à
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

/*
 * Predicado que preenche uma lista com cartas.
 * A lista de saída representa um deck de cartas que está ordenado por meio
 * do padrão estabelecido em `data`.
 */
fill_deck(15, [card(id(15), elem("water"), power(8))]) :- !.
fill_deck(Idx, [H | T]) :-
  card(id(Idx), elem(Element), power(Power)),
  H = card(id(Idx), elem(Element), power(Power)),
  NextIdx is Idx + 1,
  fill_deck(NextIdx, T).

/* 
 * Predicado que dado um índice válido e uma lista pega o 
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

/*
 * Dado uma carta, retorne o elemento dela.
 */
get_elem(card(id(_), elem(Element), power(_)), Element).

/*
 * Predicado que remove um elemento de uma lista dado o índice, note que não há 
 * verificação de índice válido.
 */
remove_at(Idx, [H|T], ReturnedList):-
    Idx =:= 0,
    ReturnedList = T,
    !.
remove_at(Idx, [H|T], ReturnedList):-
    NewIdx is Idx - 1,
    remove_at(NewIdx, T, R1),
    append([H], R1, R),
    ReturnedList = R.

/*
 * Dado um elemento, um índice válido e uma lista, substitui o elemento do dado índice 
 * pelo elemento dado na lista passada como parâmetro e retorne ela com essa modificação.
 */
sub_at(Elem, Idx, [H|T], ReturnedList):-
    (Idx =:= 0),
    append([Elem], T, NewList),
    ReturnedList = NewList,
    !.
sub_at(Elem, Idx, [H|T], ReturnedList):-
    NewIdx is Idx - 1,
    sub_at(Elem, NewIdx, T, R1),
    append([H], R1, NewList),
    ReturnedList = NewList.