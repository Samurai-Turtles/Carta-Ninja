:- consult('../models/Card.pl').
:- consult('../util/Helpers.pl').
:- consult('../util/StateManager.pl').

% Arquivo que trata das funcionalidades lógicas básicas de gameplay.

/* 
 * Predicado que recebe o índice da carta jogada e o deck, retornando
 * o deck após a jogada ser feita, com a carta jogada no final do deck.
 */
play_card(_, Idx, NewDeck):-
    (Idx < 0 ; Idx > 4),
    NewDeck = [],
    !.
play_card(Deck, Idx, NewDeck):-
    push_card_to_end(Idx, Deck, R),
    NewDeck = R.

/* 
 * Predicado que verifica qual carta dada ganhou a rodada a 
 * partir de uma comparação de seus elementos, retornando 1 para os 
 * casos que o jogador ganha, -1 quando o bot ganha e 0 para quando 
 * há empate.
 */
get_winner(card(id(IdP), elem(ElemP), power(PowerP)), card(id(IdC), elem(ElemC), power(PowerC)), R):-
    (ElemP = ElemC),
    get_winner_by_power(card(id(IdP), elem(ElemP), power(PowerP)), card(id(IdC), elem(ElemC), power(PowerC)), O),
    R is O,
    !.
get_winner(card(id(_), elem("fire"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "nature" ; ElemC = "metal"),
    R is 1,
    !.
get_winner(card(id(_), elem("metal"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "nature" ; ElemC = "earth"),
    R is 1,
    !.
get_winner(card(id(_), elem("nature"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "earth" ; ElemC = "water"),
    R is 1,
    !.
get_winner(card(id(_), elem("earth"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "fire" ; ElemC = "water"),
    R is 1,
    !.
get_winner(card(id(_), elem("water"), power(_)), card(id(_), elem(ElemC), power(_)), R):-
    (ElemC = "fire" ; ElemC = "metal"),
    R is 1,
    !.
get_winner(card(id(_), elem(_), power(_)), card(id(_), elem(_), power(_)), -1).

/*
 * Este predicado aumenta o nível de faixa do jogador em +1.
 */
level_up_player:-
    get_campaign_state(State),

    nth0(1, State, Pts),
    nth0(2, State, Lifes),
    nth0(3, State, Belt),

    update_campaign_state(Pts, Lifes, Belt + 1). 

/*
 * Esta função recebe um valor a ser somado ao número total de vidas
 * (Life Points) do jogador.
 */
update_player_life(LifeAmount):-
    get_campaign_state(State),

    nth0(1, State, Pts),
    nth0(2, State, Lifes),
    nth0(3, State, Belt),

    update_campaign_state(Pts, Lifes + LifeAmount, Belt).

/*
 * Este predicado recebe a pontuação obtida pelo jogador na partida e soma à 
 * pontuação geral da campanha até então.
 */
update_player_campaign_score(Points):-
    get_campaign_state(State),

    nth0(1, State, Pts),
    nth0(2, State, Lifes),
    nth0(3, State, Belt),

    update_campaign_state(Pts + Points, Lifes, Belt).

/*
 * Este predicado recebe um valor inteiro indicando o vencedor da rodada (1 indica
 * que o jogador venceu e -1 indica que o bot venceu), a carta vencedora e atualiza os
 * devidos dados no battle state.
 */
update_score_of(1, card(id(Id), elem(Elem), power(Power))):-
    get_current_round(Round),
    NewRound is Round + 1,
    update_round_state(NewRound),

    get_player_state(PlayerData),
    nth0(0, PlayerData, PlayerPoints),
    nth0(1, PlayerData, PlayerStreak),
    nth0(2, PlayerData, PlayerDeck),
    nth0(3, PlayerData, PlayerElemWinArray),    
    NewPlayerPoints is PlayerPoints + Power,
    NewPlayerStreak is PlayerStreak + 1,
    modify_elem_win_array(card(id(Id), elem(Elem), power(Power)), PlayerElemWinArray, NewPlayerElemWinArray), 
    update_player_state([NewPlayerPoints, NewPlayerStreak, PlayerDeck, NewPlayerElemWinArray]),
    
    get_bot_state(BotData),
    nth0(0, BotData, BotPoints),
    nth0(2, BotData, BotDeck),
    nth0(3, BotData, BotElemWinArray),
    update_bot_state([BotPoints, 0, BotDeck, BotElemWinArray]),
    
    get_extra_state(ExtraData),
    nth0(1, ExtraData, SpecialDeck),
    update_extra_state([false, SpecialDeck]),
    !.
update_score_of(1, card(id(Id), elem(Elem), power(Power))):-
    get_current_round(Round),
    NewRound is Round + 1,
    update_round_state(NewRound),

    get_player_state(PlayerData),
    nth0(0, PlayerData, PlayerPoints),
    nth0(2, PlayerData, PlayerDeck),
    nth0(3, PlayerData, PlayerElemWinArray),
    update_player_state([PlayerPoints, 0, PlayerDeck, PlayerElemWinArray]),

    get_bot_state(BotData),
    nth0(0, BotData, BotPoints),
    nth0(1, BotData, BotStreak),
    nth0(2, BotData, BotDeck),
    nth0(3, BotData, BotElemWinArray),    
    NewBotPoints is BotPoints + Power,
    NewBotStreak is BotStreak + 1,
    modify_elem_win_array(card(id(Id), elem(Elem), power(Power)), BotElemWinArray, NewBotElemWinArray),
    update_bot_state([NewBotPoints, NewBotStreak, BotDeck, NewBotElemWinArray]),

    get_extra_state(ExtraData),
    nth0(1, ExtraData, SpecialDeck),
    update_extra_state([false, SpecialDeck]),
    !.
update_score_of(0, card(id(Id), elem(Elem), power(Power))):-
    get_current_round(Round),
    NewRound is Round + 1,
    update_round_state(NewRound),

    get_player_state(PlayerData),
    nth0(0, PlayerData, PlayerPoints),
    nth0(1, PlayerData, PlayerStreak),
    nth0(2, PlayerData, PlayerDeck),
    nth0(3, PlayerData, PlayerElemWinArray),     
    update_player_state([PlayerPoints, 0, PlayerDeck, PlayerElemWinArray]),
    
    get_bot_state(BotData),
    nth0(0, BotData, BotPoints),
    nth0(2, BotData, BotDeck),
    nth0(3, BotData, BotElemWinArray),
    update_bot_state([BotPoints, 0, BotDeck, BotElemWinArray]),
    
    get_extra_state(ExtraData),
    nth0(1, ExtraData, SpecialDeck),
    update_extra_state([false, SpecialDeck]),
    !.

/*
 * Este predicado recebe uma Carta e um array de booleanos que significa as vitórias
 * de elementos na luta em questão e modifica esse array, atualizando esses valores booleanos
 * para o estado correto, retornando esse array atualizada.
 */
modify_elem_win_array(card(id(_), elem("fire"), power(_)), ElemWinArray, NewElemWinArray):-
    nth0(1, ElemWinArray, NatureWin),
    nth0(2, ElemWinArray, WaterWin),
    nth0(3, ElemWinArray, MetalWin),
    nth0(4, ElemWinArray, EarthWin),
    NewElemWinArray = [true, NatureWin, WaterWin, MetalWin, EarthWin],
    !.
modify_elem_win_array(card(id(_), elem("nature"), power(_)), ElemWinArray, NewElemWinArray):-
    nth0(0, ElemWinArray, FireWin),
    nth0(2, ElemWinArray, WaterWin),
    nth0(3, ElemWinArray, MetalWin),
    nth0(4, ElemWinArray, EarthWin),
    NewElemWinArray = [FireWin, true, WaterWin, MetalWin, EarthWin],
    !.
modify_elem_win_array(card(id(_), elem("water"), power(_)), ElemWinArray, NewElemWinArray):-
    nth0(0, ElemWinArray, FireWin),
    nth0(1, ElemWinArray, NatureWin),
    nth0(3, ElemWinArray, MetalWin),
    nth0(4, ElemWinArray, EarthWin),
    NewElemWinArray = [FireWin, NatureWin, true, MetalWin, EarthWin],
    !.
modify_elem_win_array(card(id(_), elem("metal"), power(_)), ElemWinArray, NewElemWinArray):-
    nth0(0, ElemWinArray, FireWin),
    nth0(1, ElemWinArray, NatureWin),
    nth0(2, ElemWinArray, WaterWin),
    nth0(4, ElemWinArray, EarthWin),
    NewElemWinArray = [FireWin, NatureWin, WaterWin, true, EarthWin],
    !.
modify_elem_win_array(card(id(_), elem("earth"), power(_)), ElemWinArray, NewElemWinArray):-
    nth0(0, ElemWinArray, FireWin),
    nth0(1, ElemWinArray, NatureWin),
    nth0(2, ElemWinArray, WaterWin),
    nth0(3, ElemWinArray, MetalWin),
    NewElemWinArray = [FireWin, NatureWin, WaterWin, MetalWin, true],
    !.

/* 
 * Predicado que verifica qual carta dada ganhou a rodada a partir de uma
 * comparação do poder de ambas, retornando 1 para o caso em que o jogador
 * ganha, -1 quando o bot ganha e 0 para quando há empate.
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