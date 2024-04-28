:- use_module(library(random)).
:- consult('../util/StateManager.pl').

% Arquivo que trata de todas as funcionalidades relacionadas às Cartas Especiais.


% ["swapInDeck","nullifyElement","swapBetweenHands"]
% [false, ["swapInDeck","nullifyElement","swapBetweenHands"]]


/*
 * Predicado que verifica se há o uso de cartas especiais disponível e retorna uma
 * dentre 3 possibilidades de retorno, em que 1 significa que a carta especial não foi
 * utilizada ainda, 0 denota que a carta já está em uso e -1 mostra que a carta 
 * especial já foi utilizada anteriormente.
 */
verify_special_card_availability(ReturnedValue) :-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    nth0(1, ExtraData, SpecialDeck),
    length(SpecialDeck, Length),
    (SpecialCardInUse == false, Length =:= 3),
    ReturnedValue is 1,
    !.
verify_special_card_availability(ReturnedValue) :-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    (SpecialCardInUse == true),
    ReturnedValue is 0,
    !.
verify_special_card_availability(ReturnedValue) :-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    nth0(1, ExtraData, SpecialDeck),
    length(SpecialDeck, Length),
    (SpecialCardInUse == false, Length =:= 2),
    ReturnedValue is -1.

/*
 * Predicado que define o uso da carta especial, dado um inteiro (6-8) que representa a 
 * carta special usada, fazendo as modificações necessárias no Battle State para definir 
 * que a carta dada está em uso.
 */
use_special_card(NumOfSpecialCard),
    % depois de fazer os predicados de cada carta especial faça isso aqui.
    !.

/*
 * Predicado que representa a primeira carta especial (carta número 6), em que uma carta
 * da mão do jogador é trocada aleatoriamente com uma carta selecionada de maneira randômica
 * também do deck do jogador.
 */
swap_in_own_deck:-
    get_current_round(Round),
    random(0, 5, RandomHandIndex),
    RightLimit is 16 - Round,
    random(5, RightLimit, RandomDeckIndex),

    % faça uma função que dado um elemento e um índice, troque o elemento do índice pelo
    % elemento dado.

    % Aqui pegue os elementos dos índices pelo nth0 e depois faça o swap de cada um dado
    % os índices aleatórios, aí salve o deck novo, com os get_player_state e modifique o deck
    % com o update_player_state
    !. 

/* 
 * Predicado que verifica qual carta dada ganhou a rodada a partir de uma comparação
 * do poder de ambas, retornando 1 para o caso em que o jogador ganha, -1 quando
 * o bot ganha e 0 o caso de empate, sendo a segunda carta especial (carta número 7).
 */
get_winner_by_power(card(id(_), elem(_), power(PowerP)), card(id(_), elem(_), power(PowerC)), R):-
    (PowerP > PowerC),
    R is 1,
    !.
get_winner_by_power(card(id(_), elem(_), power(PowerP)), card(id(_), elem(_), power(PowerC)), R):-
    (PowerP < PowerC),
    R is -1,
    !.
get_winner_by_power(card(id(_), elem(_), power(_)), card(id(_), elem(_), power(_)), 0).