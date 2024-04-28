% Predicados para gerenciamento de arquivos CSV

:- use_module(library(csv)).

/*
 * Retorna os dados do Ranking.
 */
read_ranking(Data) :-
    get_path("ranking.csv", Path),
    csv_read_file(Path, Data, [functor(row)]).

/*
 * Adiciona um novo registro de Rank ao arquivo.
 */
add_rank(RankName, RankPts) :-
    read_ranking(CSV),
    get_path("ranking.csv", Path),
    csv_write_file(Path, [row(RankName, RankPts) | CSV]).

/*
 * Retorna o Path do arquivo de Rankings.
 */
get_path(Filename, Path) :-
    working_directory(CWD, CWD),
    atomic_list_concat([CWD, 'app/data/', Filename], Path).

/*
 * Converte os dados CSV para uma lista de listas, onde
 * cada lista representa uma linha do arquivo.
 */
parse_rows(Rows, Lists) :-
    maplist(row_to_list, Rows, Lists).

/*
 * Converte uma linha CSV em uma lista.
 */
row_to_list(Row, List) :-
    Row =.. [row|List].
