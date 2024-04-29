:- use_module(library(random)).
:- consult('../util/StateManager.pl').
:- consult('../util/Helpers.pl').

% Arquivo que trata de todas as funcionalidades relacionadas às Cartas Especiais.

/*
 * Predicado que verifica se há o uso de cartas especiais disponível e retorna uma
 * dentre 3 possibilidades de retorno, em que 1 significa que a carta especial não foi
 * utilizada ainda, 0 denota que a carta já está em uso e -1 mostra que a carta 
 * especial já foi utilizada anteriormente.
 */
verify_special_card_availability(ReturnedValue):-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    nth0(1, ExtraData, SpecialDeck),
    length(SpecialDeck, Length),
    (SpecialCardInUse == false, Length =:= 3),
    ReturnedValue is 1,
    !.
verify_special_card_availability(ReturnedValue):-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    (SpecialCardInUse == true),
    ReturnedValue is 0,
    !.
verify_special_card_availability(ReturnedValue):-
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
use_special_card(6):-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    nth0(1, ExtraData, SpecialDeck),

    remove_at(0, SpecialDeck, NewSpecialDeck),
    swap_in_own_deck,
    update_extra_state([true, NewSpecialDeck]),
    !.
use_special_card(7):-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    nth0(1, ExtraData, SpecialDeck),

    remove_at(1, SpecialDeck, NewSpecialDeck),
    update_extra_state([true, NewSpecialDeck]),
    !.
use_special_card(8):-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    nth0(1, ExtraData, SpecialDeck),

    remove_at(2, SpecialDeck, NewSpecialDeck),
    swap_hands_cards,
    update_extra_state([true, NewSpecialDeck]).

/*
 * Aqui é retornado um valor que denota se a carta especial de anular a lógica de comparação
 * por elementos está em uso ou não, retornando true se a dita carta está em uso e false
 * se a carta não estiver em uso na rodada (esse predicado deve ser usado depois do jogador
 * jogar a carta, para utilizar a carta especial diretamente).
 */
verify_nullify_elem_card_use(ReturnedValue):-
    get_extra_state(ExtraData),
    nth0(0, ExtraData, SpecialCardInUse),
    nth0(1, ExtraData, SpecialDeck),
    ((\+member("nullifyElement", SpecialDeck)), SpecialCardInUse == true),
    ReturnedValue = true,
    !.
verify_nullify_elem_card_use(ReturnedValue):-
    ReturnedValue = false.

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

    get_player_state(PlayerData),
    nth0(2, PlayerData, PlayerDeck),
    nth0(RandomHandIndex, PlayerDeck, HandCard),
    nth0(RandomDeckIndex, PlayerDeck, DeckCard),

    sub_at(HandCard, RandomDeckIndex, PlayerDeck, IntermediateDeck),
    sub_at(DeckCard, RandomHandIndex, IntermediateDeck, NewPlayerDeck),

    nth0(0, PlayerData, PlayerScore),
    nth0(1, PlayerData, PlayerStreak),
    nth0(3, PlayerData, PlayerElemWinArray),
    update_player_state([PlayerScore, PlayerStreak, NewPlayerDeck, PlayerElemWinArray]). 

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

/*
 * Predicado que representa a carta especial de troca de cartas entre as mãos dos 
 * combatentes, em que uma carta da mão de cada lutador na batalha é escolhida 
 * aleatoriamente para que sejam trocadas entre si, sendo a terceira carta
 * especial (carta número 8).
 */
swap_hands_cards:-
    random(0, 5, RandomPlayerHandIndex),
    random(0, 5, RandomBotHandIndex),

    get_player_state(PlayerData),
    get_bot_state(BotData),
    nth0(2, PlayerData, PlayerDeck),
    nth0(2, BotData, BotDeck),
    
    nth0(RandomPlayerHandIndex, PlayerDeck, PlayerCard),
    nth0(RandomBotHandIndex, BotDeck, BotCard),

    sub_at(PlayerCard, RandomBotHandIndex, BotDeck, NewBotDeck),
    sub_at(BotCard, RandomPlayerHandIndex, PlayerDeck, NewPlayerDeck),

    nth0(0, PlayerData, PlayerScore),
    nth0(1, PlayerData, PlayerStreak),
    nth0(3, PlayerData, PlayerElemWinArray),
    update_player_state([PlayerScore, PlayerStreak, NewPlayerDeck, PlayerElemWinArray]),

    nth0(0, BotData, BotScore),
    nth0(1, BotData, BotStreak),
    nth0(3, BotData, BotElemWinArray),
    update_bot_state([BotScore, BotStreak, NewBotDeck, BotElemWinArray]).