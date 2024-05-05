:- consult('../util/StateManager.pl').
:- consult('../util/Helpers.pl').
:- consult('./Bot.pl').

/*
 * Interface que fornece a dica ao jogador.
 */
give_tip(Tip) :-
    get_player_state(PlayerState),
    nth0(0, PlayerState, Score),
    select_tip(Score, Tip).

/*
 * Seleciona o tipo de dica, de acordo com a pontuação atual da partida.
 */
select_tip(Score, Tip) :-
    Score =< 20,
    bot_selection(Card),
    tip_bot_will_play(Card, Tip),
    !.
select_tip(Score, Tip) :-
    Score =< 40,
    tip_to_play(Tip),
    !.
select_tip(_, Tip) :-
    bot_selection(Card),
    tip_not_to_play(Card, Tip),
    !.

/*
 * Utiliza o mecanismo de seleção de carta do Bot para predizer qual carta
 * o Bot tem probabilidade de jogar durante a rodada.
 */
tip_bot_will_play(Card, Tip) :-
    get_elem(Card, Element),
    translate_elem(Element, TranslatedElement),
    string_concat("O bot talvez jogue: ", TranslatedElement, Tip).

/*
 * Utiliza o mecanismo de seleção de carta do Bot para predizer, com a mão
 * do jogador, qual carta tem mais chance de vencer a rodada.
 */
tip_to_play(Tip) :-
    make_possible_choice(10, Card),
    get_elem(Card, Element),
    translate_elem(Element, TranslatedElement),
    string_concat("Você deveria jogar: ", TranslatedElement, Tip).

/*
 * Baseada na carta sorteada pelo Bot, informa ao jogador qual carta ele deveria 
 * evitar jogar durante a rodada.
 */
tip_not_to_play(Card, Tip) :-
    get_elem(Card, Element),
    not_play(Element, ElementTip),
    translate_elem(ElementTip, TranslatedElement),
    string_concat("Você não deveria jogar: ", TranslatedElement, Tip).

/*
 * Fatos auxiliares à regra tip_not_to_play
 */
not_play("fire", "nature").
not_play("nature", "water").
not_play("water", "metal").
not_play("metal", "earth").
not_play("earth", "fire").

/*
 * Chama o mecanismo de escolha do Bot para selecionar uma carta utilizando
 * o nível de dificuldade 10.
 */
bot_selection(Card) :-
    get_bot_state(BotState),
    nth0(2, BotState, BotDeck),
    slice(0, 5, BotDeck, BotHand),
    make_choice(10, CardIndex),
    get_card(CardIndex, BotHand, Card).

/*
 * Adaptação do mecanismo de seleção de carta do Bot para selecionar uma carta
 * do jogador, que tem mais chance de vencer o Bot.
 */
make_possible_choice(Level, Card) :-
    get_player_state(PlayerState),
    nth0(2, PlayerState, Deck),
    slice(0, 5, Deck, Hand),
    get_bot_state(BotState),
    get_deck_weights(Level, BotState, Weight),
    get_hand_weights(Hand, Weight, PlayerHandWeight),
    length(PlayerHandWeight, L),
    random(0, L, Choice),
    nth0(Choice, PlayerHandWeight, Id),
    nth0(Id, Hand, Card).
