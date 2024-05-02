:- consult(['./Gameplay.pl','../UI/Render.pl', '../util/Helpers.pl']).

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
menu_resolve("I") :- !.

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
    (atom_string(ValidationOut, "V") -> menu_loop; ranking_loop).
    %ranking_resolve(ValidationOut).

/*
    Define o loop da tela de créditos.
 Executa o loop da tela de créditos até uma entrada válida do player.
*/
creditos_loop :-
    action, 
    read_line(Out),
    validation_input(["V"], Out, ValidationOut),
    (atom_string(ValidationOut, "V") -> menu_loop; creditos_loop).

/*
    Define o loop que espera as informações do player para a campanha.
*/
desafiante_loop :-
    update_screen_state("desafiante"),
    action,
    read_line(Out),
    (atom_string(Out, "") -> desafiante_loop; init_campaign_state(Out)).

/*
    Define o loop da campanha do jogo.
 Define o loop para os casos de campanha em andamento, vitória do player, derrota
 do player para o bot, resolvendo os casos. 
*/
campaign_loop :-
    verify_victory(Out),
    (Out =:= 0),

    battle_loop,
    cards_comparation, % a atualização do score vem aqui, baseada na função comparationLoop em haskell.
    campaign_loop, !. 

campaign_loop :-
    verify_victory(Out),
    (Out =:= 1),

    get_player_state(PlayerData),
    get_campaign_state(CampaingData),

    nth0(0, PlayerData, PlayerScore),
    nth0(1, CampaignData, CampaignScore),
    nth0(2, CampaignData, CampaignLife),
    nth0(3, CampaignData, CampaignBelt),

    campaign_life_situation(Life),
    NextScore is PlayerScore + CampaignScore,
    NextBelt is CampaingBelt + 1,

    update_campaign_state(NextScore, Life, NextBelt),
    build_battle,
    (NextBelt =:= 6 -> campaign_win; campaign_loop), !.

campaign_loop :-
    verify_victory(Out),
    (Out =:= -1),

    get_player_state(PlayerData),
    get_campaign_state(CampaingData),

    nth0(0, PlayerData, PlayerScore),
    nth0(1, CampaignData, CampaignScore),
    nth0(2, CampaignData, CampaignLife),
    nth0(3, CampaignData, CampaignBelt),

    NextScore is PlayerScore + CampaignScore,
    Life is CampaignLife - 1,

    update_campaign_state(NextScore, Life, CampaignBelt),
    build_battle,
    (Life =< 0 -> campaign_loser; campaign_loop), !.

campaign_loop :-
    get_player_state(PlayerData),
    get_campaign_state(CampaignData),

    nth0(0, PlayerData, PlayerScore),
    nth0(1, CampaignData, CampaignScore),
    nth0(2, CampaignData, CampaignLife),
    nth0(3, CampaignData, CampaignBelt),

    campaign_life_situation(Life),
    NextScore is PlayerScore + CampaignScore,

    update_campaign_state(NextScore, Life, CampaignBelt),
    build_battle,
    campaign_loop.

/*
    Define o loop da batalha atual.
*/
battle_loop :-
    update_screen_state("batalha"),
    action,
    read_line(Out),
    validation_input(["1", "2", "3", "4", "5", "6", "7", "8", "D"], Out,  ValidationOut),

    battle_resolve(ValidationOut).

/*
    Resolve para as entradas válidas na seleção de uma carta na tela de batalha.
*/
battle_resolve(Input) :-
    member(Input, ["1", "2", "3", "4", "5"]),

    get_player_state(PlayerData),
    nth0(2, PlayerData, PlayerDeck),
    number_string(PlayerChoiceTemp, Input),
    PlayerChoice is PlayerChoiceTemp - 1,

    % bot_choice:
    get_bot_state(BotData),
    nth0(2, BotData, BotDeck),
    BotChoice = 0, % placeholder, cadê o Bot?

    play_card(PlayerDeck, PlayerChoice, NewPlayerDeck),
    play_card(BotDeck, BotChoice, NewBotDeck),

    sub_at(NewPlayerDeck, 2, PlayerData, NewPlayerData),
    sub_at(NewBotDeck, 2, BotData, NewBotData),

    update_player_state(NewPlayerData),
    update_bot_state(NewBotData), !.




% ==================== AUXILIARES ==================== %

/*
    Inicia os parâmetros iniciais de uma batalha.
*/
build_battle :-
    update_screen_state("batalha"),
    build_deck(PlayerDeck),
    build_deck(BotDeck),
    init_battle_state(PlayerDeck, BotDeck).
    
/*
    Constrói um deck de cartas permutada.
*/
build_deck(Deck) :-
    fill_deck(1, PureDeck),
    random_permutation(PureDeck, Deck).

/*
    Atualiza, se necessário, a vida do Player quando a pontuação for de 25.
*/
campaign_life_situation(Result) :-
    get_player_state(PlayerData),
    get_campaign_state(CampaingData),

    nth0(0, PlayerData, PlayerScore),
    nth0(2, CampaignData, CampaignLife),
    PossibleNewLife is CampaignLife + 1,

    (PlayerScore >= 25 -> Result = PossibleNewLife; Result = CampaignLife).


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
