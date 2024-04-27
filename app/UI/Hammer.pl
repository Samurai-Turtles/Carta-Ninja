/*
    Predicado de acesso ao processo de Replace.
 Recebe uma Lista que é a representação da tela, uma Lista que contém os elementos
 de replace e um argumento que representa a saída.
 Realiza a normalização das listas, isso significa que o predicado prepara os dois
 elementos de entrada para serem usados no predicado `forgeScreen`, não sendo 
 necessário utilizar os predicados `unlines` e `string_chars` para usar o forge.
*/
anvil(ScreenList, ReplaceList, Result) :-
    normalizedIn(ReplaceList, "", ReplaceNormalized),
    normalizedIn(ScreenList, "\n", ScreenNormalized),

    forgeScreen(ScreenNormalized, ReplaceNormalized, Out),
    Result = Out.

/*
    Predicado de substituição dos caracteres de replace.
 Realiza o replace, quando há placeholders, de informações pertinentes à tela passada.
 Recebe uma lista que contém os caracteres da tela a ser representada, uma Lista que 
 contém os elementos de replace e uma lista que será a tela forjada.
*/
forgeScreen([], L, L) :- !.
forgeScreen(BackGroundL, [], R) :-
    R = BackGroundL, !.

forgeScreen([Hback | Tback], [Hrepl | Trepl], [Hresp | Tresp]) :-
    atom_string(Hback, "#"), 
    Hresp = Hrepl,
    forgeScreen(Tback, Trepl, Tresp), !.

forgeScreen([Hback | Tback], [Hrepl | Trepl], [Hresp | Tresp]) :- 
    Hresp = Hback,
    forgeScreen(Tback, [Hrepl | Trepl], Tresp), !.


/*
    Predicado que Mescla Listas contidas em uma Lista.
 O retorno do predicado é uma lista que contém, para cada elemento da lista resposta,
 a concatenação de todos os elementos de uma determinada linha `NLine`. Esses elementos
 vêm de todas as listas internas sendo concatenadas no formato: [elem1++elem2++...++elemN].
*/
mergeControll(_, -1, []) :- !.
mergeControll(Matrix, NLine, [Hresp | Tresp]) :-
    currentNumberLine(Matrix, NLine, NumOut),
    mergeLine(Matrix, NumOut, Result),
    unlines(Result, "", NormalizedResult),

    Hresp = NormalizedResult,
    NextLine is NLine -1,
    mergeControll(Matrix, NextLine, Tresp).

% Passa por todas as listas internas e faz a concatenação dos elementos da linha `NLine`.
mergeLine([], _, []) :- !.
mergeLine([Hmatrix | Tmatrix], NLine, [Hresp | Tresp]) :-
    nth0(NLine, Hmatrix, Element),
    Hresp = Element,
    mergeLine(Tmatrix, NLine, Tresp).

% Predicado auxiliar para a correção do número da linha atual.
currentNumberLine([H | _], Line, Result) :-
    length(H, Len),
    Result is (Len - 1) - Line.

% Predicado auxiliar para a unificação dos elementos de uma lista.
unlines(Strings, Symbol, Result) :- atomic_list_concat(Strings, Symbol, Result).

/* 
 Predicado auxiliar que normaliza uma entrada contendo uma lista de 
 representações para uma lista de caracteres.
*/
normalizedIn(List, Symbol, Result) :-
    unlines(List, Symbol, ListUnlines),
    string_chars(ListUnlines, ListUnlinesChars),
    Result = ListUnlinesChars.