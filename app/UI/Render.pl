:- consult(['Hammer', 'SpritesBase', '../util/StateManager', '../core/Ranking']).

/*
 * Esta regra analisa o estado do jogo e realiza o print da respectiva tela.
 */ 
action :-
    shell(clear),
    get_screen_state(ScreenState),
    select_draw(ScreenState).

/*
 * Esta regra seleciona a regra `draw` responsável pela impressão da tela, 
 * tomado por base o estado atual.
 */
select_draw(ScreenState) :-
    atom_string("menu", ScreenState) -> draw_menu;
    atom_string("ranking", ScreenState) -> draw_ranking;
    atom_string("creditos", ScreenState) -> draw_creditos;
    atom_string("desafiante", ScreenState) -> draw_desafiante;
    atom_string("batalha", ScreenState) -> draw_batalha;
    atom_string("comparacao", ScreenState) -> draw_comparacao;
    atom_string("vitoria", ScreenState) -> draw_vitoria;
    atom_string("derrota", ScreenState) -> draw_derrota;
    atom_string("empate", ScreenState) -> draw_empate;
    atom_string("gameOver", ScreenState) -> draw_game_over;
    atom_string("gameClear", ScreenState) -> draw_game_clear;
    string_concat("Tela de estado não identificada: ", ScreenState, R),
    write(R), writeln(" não existe.").

/*
 * Esta regra imprime a tela de menu.
 */
draw_menu :- 
    screen("menu", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

/*
 * Esta regra imprime a tela de ranking.
 */
draw_ranking :-
    screen("ranking", Screen), 
    
    % Pegar os rankings.
    % TODO Lembrar de ordenar os rankings antes de pegar os 6 primeiros
    read_ranking(Rankings),
    take(6, Rankings, FirstSixRankings),
    format_rankings(FirstSixRankings, RankingsFormat),

    % Pegar a quantidade total de rankings (de 0 a 6). 
    length(RankingsFormat, RepLength),
    
    % Caracteres para complementar a representação dos rankings.
    CompLength is 138 - (23 * RepLength),
    repeat(CompLength, "=", Complete),

    % Juntar tudo em uma String só.
    append(RankingsFormat, [Complete], RepCompl),
    length(RepCompl, RepComplLength),
    LengthWorkaround is RepComplLength - 1,

    merge_controll([RepCompl], LengthWorkaround, Controll),

    anvil(Screen, Controll, ScrRanking),
    print_list(ScrRanking).
    
/*
 * Esta regra imprime a tela de créditos.
 */
draw_creditos :- 
    screen("creditos", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

/*
 * Esta regra imprime a tela do desafiante, destinada a 
 * pedir que o desafiante escreva seu nome.
*/
draw_desafiante :- 
    screen("desafiante", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

/*
 * Esta regra imprime a tela de batalha.
 */
 % ERRO
draw_batalha :-
    % Tela de batalha
    screen("batalha", Screen),

    % Pegar o valor do estado de batalha
    get_player_state(PlayerData),
    get_bot_state(BotData),
    get_extra_state(ExtraData),

    % Pegar o valor do estado da campanha
    get_campaign_state(CampaignState),

    % Pegar o deck do jogador
    nth1(1, PlayerData, PlayerDeck),

    % Pegar a representação das cartas do jogador
    current_cards(0, PlayerDeck, PlayerCardRep),

    % Pegar os dados das cartas especiais
    nth1(1, ExtraData, SpecialCardInUse),
    nth2(2, ExtraData, SpecialCardDeck),

    special_card_list(SpecialCardInUse, SpecialCardDeck, SpecialCardRep),

    % TODO Testar se alguma coisa precisa vir como matriz
    append(PlayerCardRep, SpecialCardRep, PlayerHandRep),

    % Pegar a pontuação do jogador.
    nth1(1, PlayerData, PlayerScore),
    fill_num(PlayerScore, PlayerScoreRep),

    % Pegar os elementos vitoriosos
    nth1(4, PlayerData, PlayerWinsByElement),
    used_elements(PlayerWinsByElement, ["FOGO","NATUREZA","ÁGUA","METAL","TERRA"], UsedElementsRep),

    % Pegar a pontuação do bot
    nth1(1, BotData, BotScore),
    fill_num(BotScore, BotScoreRep),

    % Pegar a quantidade de vidas do jogador
    nth1(3, CampaignState, PlayerLives),
    fill_num(PlayerLives, PlayerLivesRep),

    % Pegar a representação do rosto do bot
    face_bot(Bosses),
    nth1(4, CampaignState, BeltLevel),
    nth1(BeltLevel, Bosses, CurrentBoss),
    unlines(CurrentBoss, "", CurrentBossRep),
    
    merge_controll(PlayerHandRep, 7, PlayerHandMergeControll),

    % TODO Placeholder: dica não existe ainda.
    repeat(35, " ", TipPlaceHolder),

    % Juntar tudo numa string só.
    unlines([PlayerScoreRep, UsedElementsRep, BotScoreRep, PlayerLivesRep, CurrentBossRep, PlayerHandMergeControll, TipPlaceHolder], "", ContentChar),

    /* [LEGADO]
    string_concat(PlayerScoreRep, UsedElementsRep, ContentCharPart1),
    string_concat(ContentCharPart1, BotScoreRep, ContentCharPart2),
    string_concat(ContentCharPart2, PlayerLivesRep, ContentCharPart3),
    string_concat(ContentCharPart3, CurrentBossRep, ContentCharPart4),
    string_concat(ContentCharPart4, PlayerHandMergeControll, ContentCharPart5),
    string_concat(ContentCharPart5, TipPlaceHolder, ContentChar),
    */

    anvil(Screen, ContentChar, Result),
    writeln(Result).

/*
 * Esta regra imprime a tela de Comparação entre cartas.
 */
 % ERRO
draw_comparacao :-
    screen("coEmpate", ScrCoEmpate),
    screen("coVitoria", ScrCoVitoria),
    screen("coDerrota", ScrCoDerrota),

    % Pegar o estado da batalha.
    get_player_state(PlayerData),
    get_bot_state(BotData),
    get_extra_state(ExtraData),

    % Pegar os decks do jogador e do bot.
    nth1(3, PlayerData, PlayerDeck),
    nth1(3, BotData, BotDeck),

    % Pegar a última carta de ambos os decks.
    % 15 (tamanho fixo do deck) é hard-coded.
    nth1(15, PlayerDeck, PlayerUsedCard),
    nth1(15, BotDeck, BotUsedCard),

    % Ver se alguma carta especial está em uso e qual carta
    % especial está faltando (ou seja, sendo usada no momento).
    nth1(1, ExtraData, SpecialCardInUse),
    nth1(2, ExtraData, SpecialCardDeck),

    % Pegar o ID de ambas as cartas selecionadas.
    % Preciso utilizar `_` ou `ElemP/PowerP/ElemC/PowerC`?
    PlayerUsedCard = card(id(IdP), elem(_), power(_)),
    BotUsedCard = card(id(IdC), elem(_), power(_)),

    % Pegar o vencedor da comparação atual. A comparação
    % entre cartas é diferente caso a carta especial
    % `nullifyElement` esteja em uso.
    (
        check_null_special(SpecialCardInUse, SpecialCardDeck) ->
        % TODO Testar se funciona caso eu chame as cartas assim ou se tem que usar card({argumentos das cartas})
        get_winner_by_power(PlayerUsedCard, BotUsedCard, CardWinner);
        get_winner(PlayerUsedCard, BotUsedCard, CardWinner)
    ),

    % Pegar a representação de ambas as cartas.
    card_rep(CardRepresentations),
    nth1(IdP, CardRepresentations, PlayerCardRep),
    nth1(IdC, CardRepresentations, BotCardRep),

    merge_controll([PlayerCardRep, BotCardRep], 7, MergedCards),

    % Selecionar qual tela exibir dependendo do vencedor da
    % comparação.
    (
        CardWinner = 1 -> anvil(ScrCoVitoria, MergedCards, Result);
        CardWinner = -1 -> anvil(ScrCoDerrota, MergedCards, Result);
        anvil(ScrCoEmpate, MergedCards, Result)
    ),

    print_list(Result).

/*
 * Esta regra imprime a tela de vitória.
 */
draw_vitoria :- 
    screen("vitoria", Screen),

    formatted_campaign_score(CampaignScoreRep),

    string_chars(CampaignScoreRep, CampaignScoreChars),

    anvil(Screen, CampaignScoreChars, ScrVitoria),
    print_list(ScrVitoria).

/*
 * Esta regra imprime a tela de derrota.
 */
 % ERRO
draw_derrota :- 
    screen("derrota", Screen),

    % Pegar a pontuação da campanha do jogador.
    formatted_campaign_score(CampaignScoreRep),
    string_chars(CampaignScoreRep, CampaignScoreChars),
    
    % Pegar a quantidade de vidas do jogador e decrementá-la
    % apenas para imprimir na tela.
    nth1(3, CampaignState, PrevPlayerLives),
    CurrPlayerLives is PrevPlayerLives - 1,
    fill_num(CurrPlayerLives, FormattedPlayerLives),
    string_chars(FormattedPlayerLives, PlayerLivesRep),
    
    append(CampaignScoreChars, PlayerLivesRep, ContentChar),
    anvil(Screen, ContentChar, ScrDerrota),
    print_list(ScrDerrota).

/*
 * Esta regra imprime a tela de empate.
 */
draw_empate :- 
    screen("empate", Screen),
    /*
    % Pegar a pontuação da campanha do jogador.
    formatted_campaign_score(CampaignScoreRep),
    */
    CampaignScoreRep = "001", % Placeholder. Substituir pelo código acima.
    string_chars(CampaignScoreRep, CampaignScoreChars),

    anvil(Screen, CampaignScoreChars, ScrEmpate),
    print_list(ScrEmpate).

/*
 * Esta regra imprime a tela de gameOver
 */
draw_game_over :- 
    screen("gameOver", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

/*
 * Esta regra imprime a tela de GameClear.
 */
draw_game_clear :- 
    screen("gameClear", Screen),

    % Pegar a pontuação da campanha do jogador.
    formatted_campaign_score(CampaignScoreRep),
    
    string_chars(CampaignScoreRep, CampaignScoreChars),

    anvil(Screen, CampaignScoreChars, ScrGameClear),
    print_list(ScrGameClear).

% ========<[ Regras Auxiliares: talvez mandar todas para um arquivo utils ]>========

/*
 * Esta regra repete um caractere `Num` vezes e retorna 
 * uma string com essa repetição.
 */
repeat(Num, _, "") :- Num =< 0, !.
repeat(1, Str, Str) :- !.
repeat(Num, Str, Res):-
    Num1 is Num-1,
    repeat(Num1, Str, Res1),
    string_concat(Str, Res1, Res).

/*
 * Esta regra imprime todos os elementos de uma lista.
 */
print_list([]):-!.
print_list([H | T]) :-
    write(H),
    print_list(T).

% TODO Testar se funciona mesmo
/*
 * Esta regra dá a representação das cartas da mão do jogador.
 * É preciso chamar com um `ListIndex` 0 quando utilizá-la.
 */
current_cards(ListIndex, _, []) :- ListIndex >= 6, !.
current_cards(ListIndex, [HeadCardList | TailCardList], RepresentationList) :- 
    % Pegar o ID da carta do head (Funciona?)
    HeadCardList = card(id(IdCard), elem(_), power(_)), 

    % Pega a representação da carta correspondente ao IdCard
    card_rep(CardRepresentations),
    nth1(IdCard, CardRepresentations, CurrCard),

    % Chamada recursiva.
    NewListIndex is ListIndex + 1,
    current_cards(NewListIndex, TailCardList, RecursiveList),
    
    % Juntando numa lista só
    append([CurrCard], RecursiveList, RepresentationList). 

/*
 * Esta regra converte um número em uma representação com 2 dígitos.
 * Para números acima de 99, são utilizados apenas seus últimos 2 algarismos.
 */
fill_num(Number, NumberRep) :-
    Number >= 100,
    NewNumber is Number mod 100,
    fill_num(NewNumber, NumberRep), !.
fill_num(Number, NumberRep) :-
    Number =< 9,
    number_string(Number, NumberStr),
    string_concat("0", NumberStr, NumberRep), !.
fill_num(Number, NumberRep) :- number_string(Number, NumberRep).

/*
 * Esta regra retorna a lista de caracteres adjacentes às cartas especiais
 * na tela, dependendo da situação da batalha.
 */
special_card_list(_, SpecialCardDeck, ["", "6", "", "7", "", "8", "", ""]) :- length(SpecialCardDeck, 3), !.
special_card_list(SpecialCardInUse, SpecialCardDeck, SpecialCardRep) :-
    % Acho que para ver se é true é assim
    SpecialCardInUse,
    length(SpecialCardDeck, 2),
    special_card_check(SpecialCardDeck, SpecialCardRep),
    !.
special_card_list(_, _, ["", "X", "", "X", "", "X", "", ""]).

/*
 * Esta regra checa quais cartas especiais estão em uso e quais já foram
 * utilizadas, retornando uma lista de caracteres correspondentes a isso.
 */
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
    ].

/*
 * Esta regra prepara as entradas de substituição para cada 
 * placeholder dos elementos vitoriosos.
 */
used_elements([], _, "") :- !.
used_elements(_, [], "") :- !.
used_elements([WinsByElementHead | WinsByElementTail], [ElementNamesHead | ElementNamesTail], UsedElements) :-
    WinsByElementHead,

    used_elements(WinsByElementTail, ElementNamesTail, UsedRecursive),
    string_concat(ElementNamesHead, UsedRecursive, UsedElements), !.
used_elements([WinsByElementHead | WinsByElementTail], [ElementNamesHead | ElementNamesTail], UsedElements) :-
    \+WinsByElementHead,

    % Pegar a representação vazia do elemento
    string_length(ElementNamesHead, ElemNamesHeadLength),
    repeat(ElemNamesHeadLength, " ", BlankElement),

    used_elements(WinsByElementTail, ElementNamesTail, UsedRecursive),
    string_concat(BlankElement, UsedRecursive, UsedElements), !.

/* 
 * Esta regra checa se a carta especial `nullifyElement` está
 * sendo utilizada no turno atual.
 */
check_null_special(SpecialCardInUse, SpecialCardDeck) :-
    SpecialCardInUse,
    \+member("nullifyElement", SpecialCardDeck).

% TODO adicionar conversão para chars quando integrar com o core e remover os placeholders.
/*
 * Esta regra retorna a pontuação da campanha do jogador formatada como
 * uma String com zeros à esquerda para ocupar 3 caracteres.
 */
formatted_campaign_score(CampaignScoreRep) :-
    % Pegar o estado atual da campanha.
    get_campaign_state(CampaignState),
    
    % Pegar a pontuação total da campanha do jogador.
    nth1(2, CampaignState, CampaignScore),

    % Converter a pontuação em uma String.
    number_string(CampaignScore, CampaignScoreStr),

    % Gerar um complemento da pontuação com zeros.
    string_length(CampaignScoreStr, CampaignScoreLen),
    Len is 3 - CampaignScoreLen,
    repeat(Len, "0", Zeroes),

    % Concatenar os zeros e a pontuação numa String só.
    string_concat(Zeroes, CampaignScoreStr, CampaignScoreRep).

/*
 * Pega os primeiros `Num` elementos de uma lista.
 */
take(_, [], []) :- !.
take(0, _, []) :- !.
take(Number, [H | T], [H | T2]) :-
    NewNumber is Number - 1,
    take(NewNumber, T, T2), !.

/*
 * Esta regra formata os rankings para exibição na tela.
 */
format_rankings([], []).
format_rankings([H|T], [H2|T2]) :-
    H = [Name, Points],

    format_name(Name, FormattedName),

    number_string(Points, PointsStr),
    string_length(PointsStr, PointsStrLen),

    Len is 3 - PointsStrLen,
    repeat(Len, "0", Zeroes),
    string_concat(Zeroes, PointsStr, PointsStrRep),

    string_concat(FormattedName, PointsStrRep, H2),

    format_rankings(T, T2).

/*
 * Esta regra formata o nome de um ranking para ocupar
 * exatamente 20 caracteres.
 */
format_name(Name, FormattedName) :-
    string_length(Name, NameLen),
    (
    NameLen < 20 -> 
        (
        ComplementLen is 20 - NameLen,
        repeat(ComplementLen, " ", BlankSpaces),
        string_concat(BlankSpaces, Name, FormattedName)
        ) ;
        (
        string_chars(Name, NameChars),
        take(20, NameChars, NameCharsTrunc),
        string_chars(FormattedName, NameCharsTrunc)
        )
    ).
