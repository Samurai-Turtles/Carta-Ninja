:- consult('Hammer'), consult('SpritesBase').

% Esta função analisa o estado do jogo e realiza o print da respectiva tela.
action :-
    shell(clear), % Como verificar qual é o sistema operacional?
    % pegar o state atual 
    select_draw(menu). % Chamar o select_draw para o state.

/*
    Esta função seleciona a função `draw` responsável pela impressão da tela, 
tomado por base o estado atual.
*/
select_draw(StateScreen) :-
    atom_string("menu", StateScreen) -> draw_menu;
    atom_string("ranking", StateScreen) -> draw_ranking;
    atom_string("creditos", StateScreen) -> draw_creditos;
    atom_string("desafiante", StateScreen) -> draw_desafiante;
    atom_string("batalha", StateScreen) -> draw_batalha;
    atom_string("comparacao", StateScreen) -> draw_comparacao;
    atom_string("vitoria", StateScreen) -> draw_vitoria;
    atom_string("derrota", StateScreen) -> draw_derrota;
    atom_string("empate", StateScreen) -> draw_empate;
    atom_string("gameOver", StateScreen) -> draw_game_over;
    atom_string("gameClear", StateScreen) -> draw_game_clear;
    string_concat("StateScreen not identified: ", StateScreen, R),
    write(R), writeln(" does not exist.").

% Esta função imprime a tela de menu.
draw_menu :- 
    screen("menu", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

% Esta função imprime a tela de ranking.
draw_ranking :-
    screen("ranking", Screen), 
    % Pegar o valor do estado global
    % Formatar os primeiros 6 rankings na tela
    X = ["12345678901234567890123", "12345678901234567890123"], % "Placeholder para os rankings"

    length(X, RepLength),
    
    CompLength is 138 - (23 * RepLength),
    repeat("=", CompLength, Complete),
    
    /* [LEGADO]
    string_chars(Complete, ComplChars),
    */

    append(X, [Complete], RepCompl),
    length(RepCompl, RepComplLength),
    LengthWorkaround is RepComplLength - 1,

    merge_controll([RepCompl], LengthWorkaround, Controll),
    /*
        [LEGADO] 
    unlines(Controll, "", ControllUnlines),
    string_chars(ControllUnlines, ControllUnlinesChars),

    unlines(Screen, "\n", ScreenUnlines),
    string_chars(ScreenUnlines, ScreenUnlinesChars),

    forgeScreen(ScreenUnlinesChars, ControllUnlinesChars, RankScr),
    */
    anvil(Screen, Controll, RankScr),
    print_list(RankScr).
    
% Esta função imprime a tela de créditos.
draw_creditos :- 
    screen("creditos", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

/*
Esta função imprime a tela do desafiante, destinada a pedir
que o desafiante escrevra seu nome.
*/
draw_desafiante :- 
    screen("desafiante", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

% Esta função imprime a tela de batalha.
draw_batalha :-
    % Pegar o valor do estado de batalha
    get_player_state(PlayerData),
    % Pegar o valor do estado da campanha

    % Pegar o deck do jogador
    nth0(2, PlayerData, PlayerDeck),

    % Pegar a representação das cartas do jogador
    current_cards(0, PlayerDeck, PlayerHandRep),

    % Pegar as cartas especiais
    
    
    !.

% Esta função imprime a tela de Comparação entre cartas.
draw_comparacao :- !.

% Esta função imprime a tela de vitória.
draw_vitoria :- !.

% Esta função imprime a tela de derrota.
draw_derrota :- !.

% Esta função imprime a tela de empate.
draw_empate :- !.

% Esta função imprime a tela de gameOver
draw_game_over :- !.

% Esta função imprime a tela de GameClear.
draw_game_clear :- !.

% Funções Auxiliares: mandar para um arquivo utils

repeat(_, Num, "") :- Num =< 0, !.
repeat(Str,1,Str) :- !.
repeat(Str,Num,Res):-
    Num1 is Num-1,
    repeat(Str,Num1,Res1),
    string_concat(Str, Res1, Res).

% TODO Apagar depois
print_list([]):-!.
print_list([H | T]) :-
    write(H),
    print_list(T).

% Chamar com 0 quando for usar
% Testar se funciona mesmo
current_cards(ListIndex, _, []) :- ListIndex >= 6, !.
current_cards(ListIndex, [HeadCardList | TailCardList], RepresentationList) :- 
    % Pegar a carta do head (?)
    HeadCardList = card(id(IdCard), elem(_), power(_)), 

    % Pega a representação da carta correspondente ao IdCard
    card_rep(card_representations),
    nth0(IdCard, card_representations, CurrCard),

    % Chamada recursiva.
    NewListIndex is ListIndex + 1,
    current_cards(NewListIndex, TailCardList, RecursiveList),
    
    % Juntando numa lista só
    append([CurrCard], RecursiveList, RepresentationList). 

special_card_list(_, SpecialCardDeck, ["", "6", "", "7", "", "8", "", ""]) :- length(SpecialCardDeck, 3), !.
special_card_list(SpecialCardInUse, SpecialCardDeck, Output) :-
    % Acho que para ver se é true é assim
    SpecialCardInUse = true,
    length(SpecialCardDeck, 2),
    special_card_check(SpecialCardDeck, Output),
    !.
special_card_list(_, _, ["", "X", "", "X", "", "X", "", ""]).

special_card_check(Specials, SpecialCheck) :-
    % Tenho certeza de que tem um jeito melhor de fazer isso
    (member("swapInDeck", Specials) -> SwapInDeck = "X" ; SwapInDeck = ">"),
    (member("nullifyElement", Specials) -> NullifyElement = "X" ; NullifyElement = ">"),
    (member("swapBetweenDecks", Specials) -> SwapBetweenDecks = "X" ; SwapBetweenDecks = ">"),
    
    SpecialCheck = [
        "",
        SwapInDeck,
        "",
        NullifyElement,
        "",
        SwapBetweenDecks,
        ""
    ]
.