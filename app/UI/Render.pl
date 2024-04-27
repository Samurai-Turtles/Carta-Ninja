:- consult('Hammer'), consult('SpritesBase').

% Esta função analisa o estado do jogo e realiza o print da respectiva tela.
action :-
    shell(clear), % Como verificar qual é o sistema operacional?
    % pegar o state atual 
    selectDraw(menu). % Chamar o selectDraw para o state.

/*
    Esta função seleciona a função `draw` responsável pela impressão da tela, 
tomado por base o estado atual.
*/
selectDraw(StateScreen) :-
    atom_string("menu", StateScreen) -> drawMenu;
    atom_string("ranking", StateScreen) -> drawRanking;
    atom_string("creditos", StateScreen) -> drawCreditos;
    atom_string("desafiante", StateScreen) -> drawDesafiante;
    atom_string("batalha", StateScreen) -> drawBatalha;
    atom_string("comparacao", StateScreen) -> drawComparacao;
    atom_string("vitoria", StateScreen) -> drawVitoria;
    atom_string("derrota", StateScreen) -> drawDerrota;
    atom_string("empate", StateScreen) -> drawEmpate;
    atom_string("gameOver", StateScreen) -> drawGameOver;
    atom_string("gameClear", StateScreen) -> drawGameClear;
    string_concat("StateScreen not identified: ", StateScreen, R),
    write(R), writeln(" does not exist.").

% Esta função imprime a tela de menu.
drawMenu :- 
    screen("menu", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

% Esta função imprime a tela de ranking.
drawRank :-
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

    mergeControll([RepCompl], LengthWorkaround, Controll),
    /*
        [LEGADO] 
    unlines(Controll, "", ControllUnlines),
    string_chars(ControllUnlines, ControllUnlinesChars),

    unlines(Screen, "\n", ScreenUnlines),
    string_chars(ScreenUnlines, ScreenUnlinesChars),

    forgeScreen(ScreenUnlinesChars, ControllUnlinesChars, RankScr),
    */
    anvil(Screen, Controll, RankScr),
    printList(RankScr).
    
% Esta função imprime a tela de créditos.
drawCreditos :- 
    screen("creditos", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

/*
Esta função imprime a tela do desafiante, destinada a pedir
que o desafiante escrevra seu nome.
*/
drawDesafiante :- 
    screen("desafiante", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

% Esta função imprime a tela de batalha.
drawBatalha :-
    % Pegar o valor do estado de batalha
    get_player_state(PlayerData),
    % Pegar o valor do estado da campanha

    % Pegar o deck do jogador
    nth0(2, PlayerData, PlayerDeck),

    % Pegar a representação das cartas do jogador
    currentCards(0, PlayerDeck, PlayerHandRep),

    % Pegar as cartas especiais
    
    
    !.

% Esta função imprime a tela de Comparação entre cartas.
drawCompare :- !.

% Esta função imprime a tela de vitória.
drawVenceu :- !.

% Esta função imprime a tela de derrota.
drawDerrota :- !.

% Esta função imprime a tela de empate.
drawEmpate :- !.

% Esta função imprime a tela de gameOver
drawGameOver :- !.

% Esta função imprime a tela de GameClear.
drawGameClear :- !.

% Funções Auxiliares: mandar para um arquivo utils

repeat(_, Num, "") :- Num =< 0, !.
repeat(Str,1,Str) :- !.
repeat(Str,Num,Res):-
    Num1 is Num-1,
    repeat(Str,Num1,Res1),
    string_concat(Str, Res1, Res).

% TODO Apagar depois
printList([]):-!.
printList([H | T]) :-
    write(H),
    printList(T).

% Chamar com 0 quando for usar
% Testar se funciona mesmo
currentCards(ListIndex, _, []) :- ListIndex >= 6, !.
currentCards(ListIndex, [HeadCardList | TailCardList], RepresentationList) :- 
    % Pegar a carta do head (?)
    HeadCardList = card(id(IdCard), elem(_), power(_)), 

    % Pega a representação da carta correspondente ao IdCard
    cardRep(CardRepresentations),
    nth0(IdCard, CardRepresentations, CurrCard),

    % Chamada recursiva.
    NewListIndex is ListIndex + 1,
    currentCards(NewListIndex, TailCardList, RecursiveList),
    
    % Juntando numa lista só
    append([CurrCard], RecursiveList, RepresentationList). 

specialArr(_, SpecialCardDeck, ["", "6", "", "7", "", "8", "", ""]) :- length(SpecialCardDeck, 3), !.
specialArr(SpecialCardInUse, SpecialCardDeck, Output) :-
    % Acho que para ver se é true é assim
    SpecialCardInUse = true,
    length(SpecialCardDeck, 2),
    specialCheckUse(SpecialCardDeck, Output),
    !.
specialArr(_, _, ["", "X", "", "X", "", "X", "", ""]).

specialCheckUse(Specials, SpecialCheck) :-
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