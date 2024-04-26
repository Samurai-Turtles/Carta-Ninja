:- consult('Hammer'), consult('SpritesBase').

action :-
    shell(clear), % Como verificar qual é o sistema operacional?
    % pegar o state atual 
    selectDraw(menu). % Chamar o selectDraw para o state.

selectDraw(State) :-
    atom_string("menu", State) -> drawMenu;
    atom_string("ranking", State) -> drawRanking;
    atom_string("creditos", State) -> drawCreditos;
    atom_string("desafiante", State) -> drawDesafiante;
    atom_string("batalha", State) -> drawBatalha;
    atom_string("comparacao", State) -> drawComparacao;
    atom_string("vitoria", State) -> drawVitoria;
    atom_string("derrota", State) -> drawDerrota;
    atom_string("empate", State) -> drawEmpate;
    atom_string("gameOver", State) -> drawGameOver;
    atom_string("gameClear", State) -> drawGameClear;
    string_concat("State not identified: ", State, R),
    write(R), writeln(" does not exist.").

drawMenu :- 
    screen("menu", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

drawRank :-
    screen("ranking", Screen), 
    % Pegar o valor do estado global
    % Formatar os primeiros 6 rankings na tela
    X = ["12345678901234567890123", "12345678901234567890123"], % "Placeholder para os rankings"

    length(X, RepLength),
    
    CompLength is 138 - (23 * RepLength),
    repeat("=", CompLength, Complete),
    string_chars(Complete, ComplChars),

    append(X, [Complete], RepCompl),
    length(RepCompl, RepComplLength),
    LengthWorkaround is RepComplLength - 1,

    mergeControll([RepCompl], LengthWorkaround, Controll),
    
    unlines(Controll, "", ControllUnlines),
    string_chars(ControllUnlines, ControllUnlinesChars),

    unlines(Screen, "\n", ScreenUnlines),
    string_chars(ScreenUnlines, ScreenUnlinesChars),

    forgeScreen(ScreenUnlinesChars, ControllUnlinesChars, RankScr),
    p(RankScr).
    
drawCreditos :- 
    screen("creditos", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

drawDesafiante :- 
    screen("desafiante", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).








% Funções Auxiliares: mandar para um arquivo utils

repeat(_, Num, "") :- Num =< 0, !.
repeat(Str,1,Str) :- !.
repeat(Str,Num,Res):-
    Num1 is Num-1,
    repeat(Str,Num1,Res1),
    string_concat(Str, Res1, Res).

% TODO Apagar depois
p([]):-!.
p([H | T]) :-
    write(H),
    p(T).