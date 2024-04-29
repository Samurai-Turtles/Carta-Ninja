% Predicados para gerenciamento de arquivos CSV
:- use_module(library(csv)).

/*
 * Retorna os dados do Ranking.
 */
read_ranking(Data) :-
    get_path("ranking.csv", Path),
    csv_read_file(Path, Rows, [functor(row)]),
    parse_rows(Rows, Data).

/*
 * Adiciona um novo registro de Rank ao arquivo.
 */
add_rank(RankName, RankPts) :-
    get_path("ranking.csv", Path),
    read_ranking(RankingCSV),
    update_rank_list(RankName, RankPts, RankingCSV, TMP1),
    sort_rankings(TMP1, TMP2),
    parse_rows(TMP2, NewRankingCSV),
    csv_write_file(Path, NewRankingCSV).

/*
 * Atualiza a lista de Rankings com um novo registro.
 * 
 * Se o nome do novo registro está presente na lista, sua pontuação será atualizada.
 * Caso contrário, o registro será adicionado como uma nova lista.
 */
update_rank_list(RankName, RankPts, [], [[RankName, RankPts]]).
update_rank_list(RankName, RankPts, [[RankName | _] | Rows], [[RankName, RankPts] | Rows]) :- !.
update_rank_list(RankName, RankPts, [[X, Y] | Rows], [[X, Y] | NextRows]) :-
    update_rank_list(RankName, RankPts, Rows, NextRows).

/*
 * Ordena a lista de rankings em ordem decrescente. 
 */
sort_rankings([], []).
sort_rankings(List, Sorted) :-
    predsort(compare_ranks, List, Sorted).

/*
 * Compara dois rankings e verifica o maior.
 */
compare_ranks(Order, [_, A], [_, B]) :-
    ( A == B
    ; compare(Order, B, A)
    ).

/*
 * Retorna o Path do arquivo de Rankings.
 */
get_path(Filename, Path) :-
    working_directory(CWD, CWD),
    atomic_list_concat([CWD, 'app/data/', Filename], Path).

/*
 * Converte os dados CSV em uma lista de listas e vice-versa.
 */
parse_rows(Rows, Lists) :-
    maplist(row_to_list, Rows, Lists).

/*
 * Converte uma linha CSV em uma lista e vice-versa.
 */
row_to_list(Row, List) :-
    Row =.. [row|List].
row_to_list(List, Row) :-
    Row =.. [row|List].
