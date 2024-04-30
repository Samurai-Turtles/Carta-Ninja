:- consult('../UI/Render.pl').

/*
    Define o loop principal do Menu.
 Executa a tela de Menu enquanto recebe e valida as opções de entrada do player.
*/
menu_loop :-
    update_screen_state("menu"),
    action,
    read_line(Out),
    validation_input(["I", "R", "C", "S"], Out, ValidationOut),
    menu_resolve(ValidationOut).

/*
    Resolve o seguimento do loop para cada entrada dada pelo player.
*/
menu_resolve("I") :-
    halt.

menu_resolve("R") :-
    update_screen_state("ranking"),
    ranking_loop, !.

menu_resolve("C") :-
    update_screen_state("creditos"),
    creditos_loop, !.

menu_resolve("S") :-
    halt.

menu_resolve(_) :-
    menu_loop.

/*
    Define o loop da tela de ranking.
 Executa o loop da tela de ranking até uma entrada válida do player.
*/
ranking_loop :-
    action,
    read_line(Out),
    validation_input(["V"], Out, ValidationOut),
    ranking_resolve(ValidationOut).

/*
    Resolve o seguimento do loop para cada entrada dada pelo player.
*/
ranking_resolve("V") :-
    menu_loop, !.
ranking_resolve(_) :-
    ranking_loop.



% ==================== AUXILIARES ==================== %

/*
    Lê a entrada do usuário de modo que não precise de um ponto no final da entrada.
*/
read_line(Result) :-
    read_line_to_codes(user_input, Codes),
    string_codes(Result, Codes).

/*
    Valida se uma entrada do usuário está de acordo com as opções atuais.
 Recebe uma lista de opções válidas, uma string que representa a entrada do player
 na situação atual e um parâmetro que representa a saída.
*/
validation_input(Options, Input, Result) :-
    string_chars(Input, InputToChar),
    contains_input(Options, InputToChar, OutContains),
    length(OutContains, Total),

    LastOut is Total - 1,
    (OutContains == [] -> Result = "";
    nth0(LastOut, OutContains, OutResult), Result = OutResult).

/*
    Retorna as entradas que estão dentre as válidas para o loop atual.
 Recebe uma lista com as opções válidas, uma lista contendo os caracteres digitados
 pelo jogador e o parâmetro de saída.
*/
contains_input(_, [], []) :- !.
contains_input(Options, [HInput | TInput], [Hresult | Tresult]) :-
    string_upper(HInput, InputCurrent),
    member(InputCurrent, Options),

    Hresult = InputCurrent,
    contains_input(Options, TInput, Tresult), !.

contains_input(Options, [_ | TInput], Result) :-
    contains_input(Options, TInput, Result).
