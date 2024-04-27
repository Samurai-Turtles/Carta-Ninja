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
    string_concat("Tela de estado não identificada: ", StateScreen, R),
    write(R), writeln(" não existe.").

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

    forgeScreen(ScreenUnlinesChars, ControllUnlinesChars, ScrRanking),
    */
    anvil(Screen, Controll, ScrRanking),
    print_list(ScrRanking).
    
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

% TODO testar se funciona

% Esta função imprime a tela de batalha.
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

    % Pontuação do bot
    nth1(1, BotData, BotScore),
    fill_num(BotScore, BotScoreRep),

    % Vidas do jogador
    nth1(3, CampaignState, PlayerLives),
    fill_num(PlayerLives, PlayerLivesRep),

    % Rosto do bot
    face_bot(Bosses),
    nth1(4, CampaignState, BeltLevel),
    nth1(BeltLevel, Bosses, CurrentBoss),
    unlines(CurrentBoss, "", CurrentBossRep),
    
    merge_controll(PlayerHandRep, 7, PlayerHandMergeControll),

    % TODO Dica não existe ainda
    repeat(" ", 35, TipPlaceHolder),

    unlines([PlayerScoreRep, UsedElementsRep, BotScoreRep, PlayerLivesRep, CurrentBossRep, PlayerHandMergeControll, TipPlaceHolder], "", ContentChar),

    /*
    string_concat(PlayerScoreRep, UsedElementsRep, ContentCharPart1),
    string_concat(ContentCharPart1, BotScoreRep, ContentCharPart2),
    string_concat(ContentCharPart2, PlayerLivesRep, ContentCharPart3),
    string_concat(ContentCharPart3, CurrentBossRep, ContentCharPart4),
    string_concat(ContentCharPart4, PlayerHandMergeControll, ContentCharPart5),
    string_concat(ContentCharPart5, TipPlaceHolder, ContentChar),
    */

    anvil(Screen, ContentChar, Result),
    writeln(Result).

% TODO testar se funciona
% Esta função imprime a tela de Comparação entre cartas.
draw_comparacao :- 
    screen("coEmpate", ScrCoEmpate),
    screen("coVitoria", ScrCoVitoria),
    screen("coDerrota", ScrCoDerrota),

    get_player_state(PlayerData),
    get_bot_state(BotData),
    get_extra_state(ExtraData),

    nth1(3, PlayerData, PlayerDeck),
    nth1(3, BotData, BotDeck),

    % 15 é hard-coded. O tamanho do deck é fixo.
    nth1(15, PlayerDeck, PlayerUsedCard),
    nth1(15, BotDeck, BotUsedCard),

    % Ver se tá em uso a carta especial
    % Ver qual carta especial tá faltando
    nth1(1, ExtraData, SpecialCardInUse),
    nth1(2, ExtraData, SpecialCardDeck),

    % Underline ou ElemP/PowerP, ElemC/PowerC?
    PlayerUsedCard = card(id(IdP), elem(_), power(_)),
    BotUsedCard = card(id(IdC), elem(_), power(_)),

    (
    check_null_special(SpecialCardInUse, SpecialCardDeck) ->
    % Funciona se eu chamar as cartas assim? Ou tem que usar card(argumentos)?
    get_winner_by_power(PlayerUsedCard, BotUsedCard, CardWinner);
    get_winner(PlayerUsedCard, BotUsedCard, CardWinner)
    ),

    card_rep(CardRepresentations),
    nth1(IdP, CardRepresentations, PlayerCardRep),
    nth1(IdC, CardRepresentations, BotCardRep),

    merge_controll([PlayerCardRep, BotCardRep], 7, MergedCards),

    (
    CardWinner = 1 -> anvil(ScrCoVitoria, MergedCards, Result);
    CardWinner = -1 -> anvil(ScrCoDerrota, MergedCards, Result);
    anvil(ScrCoEmpate, MergedCards, Result)
    ),

    print_list(Result).

% Esta função imprime a tela de vitória.
draw_vitoria :- 
    screen("vitoria", Screen),
    /*
    get_campaign_state(CampaignState),
    
    nth1(2, CampaignState, CampaignScore),

    number_string(CampaignScore, CampaignScoreStr),
    string_length(CampaignScoreStr, CampaignScoreLen),
    Len is 3 - CampaignScoreLen,

    repeat("0", Len, Zeroes),
    string_concat(Zeroes, CampaignScoreStr, CampaignScoreRep),
    string_chars(CampaignScoreRep, CampaignScoreChars),
    */

    % TODO Tirar isso depois, quando integrar com o core. Substituir pelo código acima
    ScorePlaceholder = "001",
    string_chars(ScorePlaceholder, CampaignScoreChars),

    anvil(Screen, CampaignScoreChars, ScrVitoria),
    print_list(ScrVitoria).

% Esta função imprime a tela de derrota.
draw_derrota :- 
    screen("derrota", Screen),

    /*
    get_campaign_state(CampaignState),
    
    nth1(2, CampaignState, CampaignScore),

    number_string(CampaignScore, CampaignScoreStr),
    string_length(CampaignScoreStr, CampaignScoreLen),
    Len is 3 - CampaignScoreLen,

    repeat("0", Len, Zeroes),
    string_concat(Zeroes, CampaignScoreStr, CampaignScoreRep),
    string_chars(CampaignScoreRep, CampaignScoreChars),
    
    nth1(3, CampaignState, PrevPlayerLives),
    CurrPlayerLives is PrevPlayerLives - 1,
    fill_num(CurrPlayerLives, FormattedPlayerLives),
    string_chars(FormattedPlayerLives, PlayerLivesRep),
    */

    % TODO Tirar isso depois, quando integrar com o core. Substituir pelo código acima
    ScorePlaceholder = "001",
    string_chars(ScorePlaceholder, CampaignScoreChars),
    LivesPlaceholder = "05",
    string_chars(LivesPlaceholder, PlayerLivesRep),
    
    append(CampaignScoreChars, PlayerLivesRep, ContentChar),
    anvil(Screen, ContentChar, ScrDerrota),
    print_list(ScrDerrota).

% Esta função imprime a tela de empate.
draw_empate :- 
    screen("empate", Screen),
    /*
    get_campaign_state(CampaignState),
    
    nth1(2, CampaignState, CampaignScore),

    number_string(CampaignScore, CampaignScoreStr),
    string_length(CampaignScoreStr, CampaignScoreLen),
    Len is 3 - CampaignScoreLen,

    repeat("0", Len, Zeroes),
    string_concat(Zeroes, CampaignScoreStr, CampaignScoreRep),
    string_chars(CampaignScoreRep, CampaignScoreChars),
    */

    % TODO Tirar isso depois, quando integrar com o core. Substituir pelo código acima
    ScorePlaceholder = "001",
    string_chars(ScorePlaceholder, CampaignScoreChars),

    anvil(Screen, CampaignScoreChars, ScrEmpate),
    print_list(ScrEmpate).

% Esta função imprime a tela de gameOver
draw_game_over :- 
    screen("gameOver", Screen), 
    unlines(Screen, "\n", Result),
    writeln(Result).

% Esta função imprime a tela de GameClear.
draw_game_clear :- 
    screen("gameClear", Screen),
    /*
    get_campaign_state(CampaignState),
    
    nth1(2, CampaignState, CampaignScore),

    number_string(CampaignScore, CampaignScoreStr),
    string_length(CampaignScoreStr, CampaignScoreLen),
    Len is 3 - CampaignScoreLen,

    repeat("0", Len, Zeroes),
    string_concat(Zeroes, CampaignScoreStr, CampaignScoreRep),
    string_chars(CampaignScoreRep, CampaignScoreChars),
    */

    % TODO Tirar isso depois, quando integrar com o core. Substituir pelo código acima
    ScorePlaceholder = "001",
    string_chars(ScorePlaceholder, CampaignScoreChars),

    anvil(Screen, CampaignScoreChars, ScrGameClear),
    print_list(ScrGameClear).

% Funções Auxiliares: mandar para um arquivo utils

% Repete um caractere `Num` vezes e retorna uma string com
% essa repetição.
repeat(_, Num, "") :- Num =< 0, !.
repeat(Str, 1, Str) :- !.
repeat(Str, Num, Res):-
    Num1 is Num-1,
    repeat(Str, Num1, Res1),
    string_concat(Str, Res1, Res).

% TODO Apagar depois
% Esta função imprime todos os elementos de uma lista.
print_list([]):-!.
print_list([H | T]) :-
    write(H),
    print_list(T).

% Chamar com 0 quando for usar
% Testar se funciona mesmo

% Esta função dá a representação das cartas da mão do jogador.
% É preciso chamar com um ListIndex 0 quando utilizá-la.
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

% Esta função converte um número em uma representação com 2 dígitos.
% Para números acima de 99, são utilizados apenas seus últimos 2 algarismos.
fill_num(Number, NumberRep) :-
    Number >= 100,
    NewNumber is Number mod 100,
    fill_num(NewNumber, NumberRep), !.
fill_num(Number, NumberRep) :-
    Number =< 9,
    number_string(Number, NumberStr),
    string_concat("0", NumberStr, NumberRep), !.
fill_num(Number, NumberRep) :- number_string(Number, NumberRep).

% Esta função retorna a lista de caracteres adjacentes às cartas especiais
% na tela, dependendo da situação da batalha.
special_card_list(_, SpecialCardDeck, ["", "6", "", "7", "", "8", "", ""]) :- length(SpecialCardDeck, 3), !.
special_card_list(SpecialCardInUse, SpecialCardDeck, SpecialCardRep) :-
    % Acho que para ver se é true é assim
    SpecialCardInUse,
    length(SpecialCardDeck, 2),
    special_card_check(SpecialCardDeck, SpecialCardRep),
    !.
special_card_list(_, _, ["", "X", "", "X", "", "X", "", ""]).

% Esta função checa quais cartas especiais estão em uso e quais já
% foram utilizadas, retornando uma lista de caracteres correspondentes
% a isso.
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

% Esta função prepara as entradas de substituição para cada 
% placeholder dos elementos vitoriosos.
used_elements([], _, "") :- !.
used_elements(_, [], "") :- !.
used_elements([WinsByElementHead | WinsByElementTail], [ElementNamesHead | ElementNamesTail], UsedElements) :-
    WinsByElementHead,

    % Pegar a representação vazia do elemento
    % string_length(ElementNamesHead, ElemNamesHeadLength),
    % repeat(" ", ElemNamesHeadLength, BlankElement),

    used_elements(WinsByElementTail, ElementNamesTail, UsedRecursive),
    string_concat(ElementNamesHead, UsedRecursive, UsedElements), !.
used_elements([WinsByElementHead | WinsByElementTail], [ElementNamesHead | ElementNamesTail], UsedElements) :-
    \+WinsByElementHead,

    % Pegar a representação vazia do elemento
    string_length(ElementNamesHead, ElemNamesHeadLength),
    repeat(" ", ElemNamesHeadLength, BlankElement),

    used_elements(WinsByElementTail, ElementNamesTail, UsedRecursive),
    string_concat(BlankElement, UsedRecursive, UsedElements), !.

check_null_special(SpecialCardInUse, SpecialCardDeck) :-
    SpecialCardInUse,
    \+member("nullifyElement", SpecialCardDeck).
