:- consult('../util/StateManager.pl').

/*
 * Posições dos elementos no array de pesos.
 */
index_elem("fire", 0).
index_elem("nature", 1).
index_elem("water", 2).
index_elem("metal", 3).
index_elem("earth", 4).

/*
 * Pesos padrão para cada elemento.
 *
 * Para cada elemento, a lista incrementa o peso dos elementos fortes contra o dado elemento
 * e decrementa o peso dos elementos fracos contra o mesmo.
 */
array_elem("fire", [2,4,1,3]).
array_elem("nature", [0,3,2,4]).
array_elem("water", [1,4,0,3]).
array_elem("metal", [0,2,1,4]).
array_elem("earth", [1,3,0,2]).

/* 
 * Predicado que seleciona o index de uma das cartas da mão do bot
 */
make_choice(BeltLevel, Id) :-
    % Pega o Deck do Bot
    get_bot_state(BotState),
    nth0(2, BotState, Deck),

    % Extrai a mão do Bot
    slice(0, 4, Deck, Hand),
    get_player_state(PlayerState),
    
    % Determina os pesos dos elements no Deck e da mão do jogador
    get_deck_weights(BeltLevel, PlayerState, DeckWeights),
    get_hand_weights(Hand, DeckWeights, HandWeights),
    
    % Realiza a seleção de uma carta da mão baseada nos pesos
    length(HandWeights, L),
    random(0, L, Choice),
    nth0(Choice, HandWeights, Id).

/*
 * Retorna os pesos de cada elemento conforme as cartas presentes no Deck.
 * 
 * Os valores na lista de retorna estão dispostos na ordem abaixo:
 * [Fogo, Natureza, Água, Metal, Terra]
 */
get_deck_weights(BeltLevel, PlayerState, Weight) :-
    % Cria uma lista representando os pesos iniciais
    ConstValue is 1 + (6 * BeltLevel),
    create_array(ConstValue, 5, CurrentWeight),
    
    % Determina os pesos dos elementos no Deck do jogador
    nth0(2, PlayerState, Deck),
    get_deck_weights_recursive(Deck, BeltLevel, CurrentWeight, Weight).

/*
 * Varre todo o Deck, modificando os pesos de acordo com as cartas encontradas.
 */
get_deck_weights_recursive([], _, CurrentWeight, CurrentWeight) :- !.
get_deck_weights_recursive([card(id(_), elem(E), power(_)) | T], BeltLevel, CurrentWeight, Weights) :-
    array_elem(E, Array),
    update_weight(BeltLevel, Array, CurrentWeight, NewWeights),
    get_deck_weights_recursive(T, BeltLevel, NewWeights, Weights),
    !.

/*
 * Atualiza a lista de pesos.
 */
update_weight(BeltLevel, ElemWeights, CurrentWeight, NewWeight) :-
    % Pega os valores da lista de pesos do elemento
    nth1(1, ElemWeights, Increase1),
    nth1(2, ElemWeights, Increase2),
    nth1(3, ElemWeights, Decrease1),
    nth1(4, ElemWeights, Decrease2),

    aux_update_weight(BeltLevel, Increase1, CurrentWeight, New1),
    aux_update_weight(BeltLevel, Increase2, New1, New2),
    NegativeI is BeltLevel * (-1),
    aux_update_weight(NegativeI, Decrease1, New2, New3),
    aux_update_weight(NegativeI, Decrease2, New3, NewWeight).

aux_update_weight(BeltLevel, Idx, Current, New) :-
    nth0(Idx, Current, Increase),
    ValueIncrease is Increase + BeltLevel,
    update_array(Current, Idx, ValueIncrease, New).
    
/*
 * Atualiza o valor de uma posição específica da lista.
 */
update_array([_ | T], 0, NewValue, [NewValue | T]) :- !.
update_array([H | T], Idx, NewValue, [H | Return]) :-
    NewIdx is Idx - 1,
    update_array(T, NewIdx, NewValue, Return).

/*
 * Cria uma lista com um valor repetido N vezes.
 */
create_array(_, 0, []).
create_array(Value, Iteration, [Value | T]) :-
    Iteration > 0,
    NextIteration is Iteration - 1,
    create_array(Value, NextIteration, T).

/*
 * Monta uma lista onde cada índice de 0 a 4 é repetido um número de vezes igual
 * ao peso do elemento naquela posição na mão do Bot.
 */
get_hand_weights(Hand, WeightList, WeightHand) :-
    get_hand_weights_recursive(0, Hand, WeightList, WeightHand).

get_hand_weights_recursive(5, _, _, []) :- !.
get_hand_weights_recursive(Idx, [card(id(_), elem(Elem), power(_)) | T], WeightList, Return) :-
    index_elem(Elem, ElemIdx),
    nth0(ElemIdx, WeightList, N),
    create_array(Idx, N, List),
    NextIdx is Idx + 1 ,
    get_hand_weights_recursive(NextIdx, T, WeightList, WeightHand),
    append(List, WeightHand, Return).

/*
 * Predicado slice auxiliar.
 */
slice(0, 0, [H | _], [H]) :- !.
slice(0, End, [H | T], [H | Return]) :-
    End > 0,
    NewEnd is End - 1,
    slice(0, NewEnd, T, Return),
    !.
slice(Start, End, [_ | T], Return) :-
    NewStart is Start - 1,
    slice(NewStart, End, T, Return).
