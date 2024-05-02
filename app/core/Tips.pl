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
    tipNotToPlay(Card, Tip),
    !.

tipBotWillPlay(Card, Tip):-
    get_elem(Card, Element),
    translate_elem(Element, TElement)
    string_concat("O bot talvez jogue ", TElement, Tip).

tipNotToPlay(Card, Tip):-
    get_elem(Card, Element),
    notPlay(Element, ElementTip),
    translate_elem(ElementTip, TElement),
    string_concat("Você não deveria jogar ", TElement, Tip).

notPlay(fire, nature).
notPlay(nature, water).
notPlay(water, metal).
notPlay(metal, earth).
notPlay(earth, fire).


botSelection(Card):-
    get_bot_state(BotState),
    nth0(2, BotState, BotDeck),
    slice(0, 4, BotDeck, BotHand),
    makeChoice(10, CardIndex),
    get_card(CardIndex, BotHand, Card).


    



