:- consult('../util/StateManager.pl').
:- consult('../util/Helpers.pl').
:- consult('./Bot.pl').

giveTip(Tip):-
    get_player_state(PlayerState),
    nth0(0, PlayerState, Score),
    .

selectTip(Score, Tip):-
    Score =< 20,
    tipBotWillPlay(),
    !.
selectTip(Score, Tip):-
    Score =< 40,
    tipToPlay(),
    !.
selectTip(_, Tip):-
    tipNotToPlay(),
    !.

tipBotWillPlay(Tip):-
    get_bot_state(BotState),


