% Predicados para gerenciamento de arquivos CSV
:- use_module(library(csv)).

/*
 * Retorna os dados do Ranking em uma lista de tuplas (listas),
 * onde cada tupla segue o formato [Rank, Points].
 */
read_ranking(Data) :-
    get_ranking_filepath(Path),
    csv_read_file(Path, Rows, [functor(row)]),
    parse_rows(Rows, Data),
	!.

/*
 * Recebe os dados de uma campanha e as grava no Ranking. A
 * lista é gravada de forma ordenada e decrescente.
 */
add_rank(Name, Points) :-
    % Puxa os dados existentes no CSV
    read_ranking(RankingCSV),

    % Adiciona os novos dados à lista de ranks
    atom_string(Name, ParsedName),
    insert_rank(ParsedName, Points, RankingCSV, TMP1),
    
    % Ordena a lista de ranks e a converte para linhas CSV
    sort_rankings(TMP1, TMP2),
    parse_rows(TMP2, NewRankingCSV),

    % Escreve a lista atualizada no arquivo
    get_ranking_filepath(Path),
    csv_write_file(Path, NewRankingCSV),
	!.

% ====================== Auxiliares ====================== %

/*
 * Recebe os dados da campanha a ser gravada na lista e retorna a
 * nova lista com os dados inseridos.
 *
 * O registro da campanha pode ocorrer de duas maneiras:
 *
 *  1º) Existe uma campanha com mesmo nome na lista:
 *      permanece a maior pontuação dentre os registros
 *  2º) Não existe uma campanha com o mesmo nome na lista:
 *      uma nova tupla é adicionada ao final da lista *
 *
 */
insert_rank(Name, Points, [], [[Name, Points]]) :- !.
insert_rank(Name, Points, [[Name, RankPts] | List], [[Name, X] | List]) :-
    ( Points > RankPts
    -> X = Points
    ; X = RankPts
    ), !.
insert_rank(Name, Points, [Rank | Rows], [Rank | NextRows]) :-
    insert_rank(Name, Points, Rows, NextRows).

/*
 * Ordena a lista de rankings em ordem decrescente. 
 */
sort_rankings([], []) :- !.
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
 * Retorna o caminho absoluto do arquivo de Ranking.
 */
get_ranking_filepath(Path) :-
    % Pega o caminho absoluto do diretório atual e converte-o em uma lista
    working_directory(WorkDir, WorkDir),
    split_string(WorkDir, "/", "/", TMP1),

    % Concatena o caminho do arquivo CSV ao caminho do diretório raíz do projeto
    get_project_path("app", TMP1, ProjectDir),
    atomic_list_concat(["/", ProjectDir, "app/data/ranking.csv"], TMP2),
    atom_string(TMP2, Path),
	!.

/*
 * Retorna o caminho absoluto do diretório raíz do projeto.
 */
get_project_path(_, [], "").
get_project_path(DirName, [DirName | _], "") :- !.
get_project_path(DirName, [H | T], Path) :-
    get_project_path(DirName, T, TMP1),
    atomic_list_concat([H, TMP1], "/", TMP2),
    atom_string(TMP2, Path).

/*
 * Converte os dados CSV em uma lista de listas e vice-versa.
 */
parse_rows(Rows, Lists) :-
    maplist(row_to_list, Rows, TMP),
    maplist(name_to_string, TMP, Lists).
parse_rows(Lists, Rows) :-
    maplist(name_to_string, TMP, Lists),
    maplist(row_to_list, Rows, TMP).

/*
 * Converte o nome do Rank (atom) para uma string e vice-versa.
 */
name_to_string([Name, Points], [String, Points]) :- 
    atom_string(Name, String), !.
name_to_string([String, Points], [Name, Points]) :-
    atom_string(String, Name), !.

/*
 * Converte uma linha CSV em uma lista e vice-versa.
 */
row_to_list(Row, List) :-
    Row =.. [row | List].
row_to_list(List, Row) :-
    Row =.. [row | List].
