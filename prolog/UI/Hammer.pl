/*
    Predicado de acesso ao processo de Replace.
 Recebe uma Lista que é a representação da tela, uma Lista que contém os elementos
 de replace e um argumento que representa a saída.
 Realiza a normalização das listas, isso significa que o predicado prepara os dois
 elementos de entrada para serem usados no predicado `forge_screen`, não sendo 
 necessário utilizar os predicados `unlines` e `string_chars` para usar o forge.
*/
anvil(ScreenList, ReplaceList, Result) :-
    normalized_in(ReplaceList, "", ReplaceNormalized),
    normalized_in(ScreenList, "\n", ScreenNormalized),

    forge_screen(ScreenNormalized, ReplaceNormalized, Out),
    Result = Out.

/*
    Predicado de substituição dos caracteres de replace.
 Realiza o replace, quando há placeholders, de informações pertinentes à tela passada.
 Recebe uma lista que contém os caracteres da tela a ser representada, uma Lista que 
 contém os elementos de replace e uma lista que será a tela forjada.
*/
forge_screen([], L, L) :- !.
forge_screen(BackGroundL, [], R) :-
    R = BackGroundL, !.

forge_screen([Hback | Tback], [Hrepl | Trepl], [Hresp | Tresp]) :-
    atom_string(Hback, "#"), 
    Hresp = Hrepl,
    forge_screen(Tback, Trepl, Tresp), !.

forge_screen([Hback | Tback], [Hrepl | Trepl], [Hresp | Tresp]) :- 
    Hresp = Hback,
    forge_screen(Tback, [Hrepl | Trepl], Tresp), !.


/*
    Predicado que Mescla Listas contidas em uma Lista.
 O retorno do predicado é uma lista que contém, para cada elemento da lista resposta,
 a concatenação de todos os elementos de uma determinada linha `NLine`. Esses elementos
 vêm de todas as listas internas sendo concatenadas no formato: [elem1++elem2++...++elemN].
*/
merge_controll(_, -1, []) :- !.
merge_controll(Matrix, NLine, [Hresp | Tresp]) :-
    current_number_line(Matrix, NLine, NumOut),
    merge_line(Matrix, NumOut, Result),
    unlines(Result, "", NormalizedResult),

    Hresp = NormalizedResult,
    NextLine is NLine -1,
    merge_controll(Matrix, NextLine, Tresp).

% Passa por todas as listas internas e faz a concatenação dos elementos da linha `NLine`.
merge_line([], _, []) :- !.
merge_line([Hmatrix | Tmatrix], NLine, [Hresp | Tresp]) :-
    nth0(NLine, Hmatrix, Element),
    Hresp = Element,
    merge_line(Tmatrix, NLine, Tresp).

% Predicado auxiliar para a correção do número da linha atual.
current_number_line([H | _], Line, Result) :-
    length(H, Len),
    Result is (Len - 1) - Line.

% Predicado auxiliar para a unificação dos elementos de uma lista.
unlines(Strings, Symbol, Result) :- atomic_list_concat(Strings, Symbol, Result).

/* 
 Predicado auxiliar que normaliza uma entrada contendo uma lista de 
 representações para uma lista de caracteres.
*/
normalized_in(List, Symbol, Result) :-
    unlines(List, Symbol, ListUnlines),
    string_chars(ListUnlines, ListUnlinesChars),
    Result = ListUnlinesChars.