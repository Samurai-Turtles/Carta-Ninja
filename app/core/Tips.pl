:- consult('../util/StateManager.pl').
:- consult('../util/Helpers.pl').
:- consult('./Bot.pl').

giveTip(Tip):-
    get_player_state(PlayerState),
    nth0(0, PlayerState, Score),
    selectTip(Score, Tip).

selectTip(Score, Tip):-
    Score =< 20,
    botSelection(Card),
    tipBotWillPlay(Card, Tip),
    !.
selectTip(Score, Tip):-
    Score =< 40,
    tipToPlay(),
    !.
selectTip(_, Tip):-
    tipNotToPlay(),
    !.

tipBotWillPlay(Card, Tip):-
    get_elem(Card, Element),
    translate_elem(Element, TElement)
    string_concat("O bot talvez jogue ", TElement, Tip).

botSelection(Card):-
    get_bot_state(BotState),
    nth0(2, BotState, BotDeck),
    slice(0, 4, BotDeck, BotHand),
    makeChoice(10, CardIndex),
    get_card(CardIndex, BotHand, Card).


    



