:- consult('../util/StateManager.pl').
:- consult('../util/Helpers.pl').
:- consult('./Bot.pl').

/*
 * Interface que fornece a dica ao jogador.
 */
giveTip(Tip):-
    get_player_state(PlayerState),
    nth0(0, PlayerState, Score),
    selectTip(Score, Tip).

/*
 * Seleciona o tipo de dica, de acordo com a pontuação
 * atual da partida.
 */
selectTip(Score, Tip):-
    Score =< 20,
    botSelection(Card),
    tipBotWillPlay(Card, Tip),
    !.
selectTip(Score, Tip):-
    Score =< 40,
    tipToPlay(Tip),
    !.
selectTip(_, Tip):-
    botSelection(Card),
    tipNotToPlay(Card, Tip),
    !.

/*
 * Utiliza o mecanismo de seleção de carta do bot para predizer, qual carta
 * o bot tem probabilidade de jogar durante a rodada.
 */
tipBotWillPlay(Card, Tip):-
    get_elem(Card, Element),
    translate_elem(Element, TElement),
    string_concat("O bot talvez jogue: ", TElement, Tip).

/*
 * Utiliza o mecanismo de seleção de carta do bot para predizer, com a mão
 * do jogador, qual carta tem mais chance de vencer a rodada.
 */
tipToPlay(Tip):-
    makePossibleChoice(10, Card),
    get_elem(Card, Element),
    translate_elem(Element, TElement),
    string_concat("Você deveria jogar: ", TElement, Tip).

/*
 * Baseada na carta sorteada pelo bot, informa ao jogador qual carta ele deveria 
 * evitar jogar durante a rodada.
 */
tipNotToPlay(Card, Tip):-
    get_elem(Card, Element),
    notPlay(Element, ElementTip),
    translate_elem(ElementTip, TElement),
    string_concat("Você não deveria jogar: ", TElement, Tip).

/*
 * Fatos auxiliares a regra tipNotToPlay
 */
notPlay("fire", "nature").
notPlay("nature", "water").
notPlay("water", "metal").
notPlay("metal", "earth").
notPlay("earth", "fire").

/*
 * Chama o mecanismo de escolha do bot para selecionar uma carta
 * utilizando o nível de dificuldade 10.
 */
botSelection(Card):-
    get_bot_state(BotState),
    nth0(2, BotState, BotDeck),
    slice(0, 5, BotDeck, BotHand),
    makeChoice(10, CardIndex),
    get_card(CardIndex, BotHand, Card).

/*
 * Adaptação do mecanismo de seleção de carta do bot para selecionar
 * uma carta do jogador, que tem mais chance de vencer o bot.
 */
makePossibleChoice(Level, Card):-
    get_player_state(PlayerState),
    nth0(2,PlayerState,Deck),
    slice(0, 5, Deck, Hand),
    get_bot_state(BotState),
    wDeck(Level,BotState,Weight),
    wHand(Hand,Weight, W_player_hand),
    length(W_player_hand, L),
    random(0,L,Choice),
    nth0(Choice,W_player_hand,Id),
    nth0(Id,Hand,Card).




