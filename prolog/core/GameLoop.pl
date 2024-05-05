:- consult(['./Gameplay.pl', './Bot.pl', '../UI/Render.pl', './Ranking.pl', '../util/Helpers.pl']).

/*
 * Define o predicado que mantém o loop central do Carta Ninja.
 */
init_loop :-
    init_screen_state,

    menu_loop,
    desafiante_loop,
    build_battle,
    campaign_stage,
    ranking_final,
    init_loop.

/*
 * Define o loop principal do Menu.
 * 
 * Executa a tela de Menu enquanto recebe e valida as opções de entrada do player.
 */
menu_loop :-
    update_screen_state("menu"),
    action,
    read_line(Out),
    validation_input(["I", "R", "C", "S"], Out, ValidationOut),
    menu_resolve(ValidationOut).

/*
 * Resolve o seguimento do loop para cada entrada dada pelo player.
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
 * Define o loop da tela de ranking.
 * 
 * Executa o loop da tela de ranking até uma entrada válida do player.
*/
ranking_loop :-
    action,
    read_line(Out),
    validation_input(["V"], Out, ValidationOut),
    (atom_string(ValidationOut, "V") -> menu_loop; ranking_loop).

/*
 * Define o loop da tela final de ranking.
 * 
 * Executa o loop da tela de ranking até uma entrada válida do player.
 */
ranking_final :-
    update_screen_state("ranking"),
    action,
    read_line(Out),
    validation_input(["V"], Out, ValidationOut),
    (atom_string(ValidationOut, "V") -> true; ranking_final).

/*
 * Define o loop da tela de créditos.
 *
 * Executa o loop da tela de créditos até uma entrada válida do player.
 */
creditos_loop :-
    action, 
    read_line(Out),
    validation_input(["V"], Out, ValidationOut),
    (atom_string(ValidationOut, "V") -> menu_loop; creditos_loop).

/*
 * Define o loop que espera as informações do player para a campanha.
 */
desafiante_loop :-
    update_screen_state("desafiante"),
    action,
    read_line(Out),
    (atom_string(Out, "") -> desafiante_loop; init_campaign_state(Out)).

/*
 * Define o predicado que controla o estágio da campanha.
 */
campaign_stage :-
    verify_victory(Out),
    campaign_loop(Out).

/*
 * Define o loop da campanha do jogo.
 * 
 * Define o loop para os casos de campanha em andamento, vitória do player, 
 * derrota do player para o bot, resolvendo os casos. 
*/
campaign_loop(0) :-
    battle_stage,
    comparation_stage,
    campaign_stage, !. 

campaign_loop(1) :-
    get_player_state(PlayerData),

    nth0(0, PlayerData, PlayerScore),

    campaign_life_situation(PlayerData),
    update_player_campaign_score(PlayerScore),

    level_up_player,
    build_battle,
    get_campaign_state(NewCampaignData),
    nth0(3, NewCampaignData, NextBelt),
    (NextBelt >= 6 -> campaign_clear; battle_win), !.

campaign_loop(-1) :-
    get_player_state(PlayerData),
    nth0(0, PlayerData, PlayerScore),

    update_player_life(-1),
    update_player_campaign_score(PlayerScore),

    build_battle,
    get_campaign_state(CampaignData),
    nth0(2, CampaignData, PlayerLife),

    (PlayerLife =< 0 -> campaign_end; battle_defeat), !.

campaign_loop(-2) :-
    get_player_state(PlayerData),

    nth0(0, PlayerData, PlayerScore),

    campaign_life_situation(PlayerData),
    update_player_campaign_score(PlayerScore),

    build_battle,
    battle_draw.

/*
 * Define o estágio da batalha atual.
 */
battle_stage :-
    update_screen_state("batalha"),
    action,

    read_line(Out),
    validation_input(["1", "2", "3", "4", "5", "6", "7", "8", "D"], Out,  ValidationOut),
    
    verify_special_card_availability(SpecialSituation),
    battle_resolve(ValidationOut, SpecialSituation).

/*
 * Resolve para as entradas válidas na seleção de uma carta na tela de batalha.
 */
battle_resolve(Input, _) :-
    member(Input, ["1", "2", "3", "4", "5"]),

    get_player_state(PlayerData),
    nth0(2, PlayerData, PlayerDeck),
    number_string(PlayerChoiceTemp, Input),
    PlayerChoice is PlayerChoiceTemp - 1,

    get_bot_state(BotData),
    get_campaign_state(CampaignData),
    nth0(2, BotData, BotDeck),
    nth0(3, CampaignData, BeltLevel),
    make_choice(BeltLevel, BotChoice),

    play_card(PlayerDeck, PlayerChoice, NewPlayerDeck),
    play_card(BotDeck, BotChoice, NewBotDeck),

    sub_at(NewPlayerDeck, 2, PlayerData, NewPlayerData),
    sub_at(NewBotDeck, 2, BotData, NewBotData),

    update_player_state(NewPlayerData),
    update_bot_state(NewBotData), !.

battle_resolve("6", 1) :-
    use_special_card(6),
    battle_stage, !.

battle_resolve("7", 1) :-
    use_special_card(7),
    battle_stage, !.

battle_resolve("8", 1) :-
    use_special_card(8),
    battle_stage, !.

battle_resolve("D", _) :-
    get_tip_avaliable("PRESSIONE [D] PARA USAR A DICA"),
    update_tip_avaliable("DICA EM USO"),
    battle_stage, !.

battle_resolve(_, _) :-
    battle_stage.

/*
 * Define o estágio de comparação entre cartas durante uma batalha.
 */
comparation_stage :-
    verify_nullify_elem_card_use, 
    call_screen("comparacao"),
    
    get_last_cards(PlayerCard, BotCard),
    get_winner_by_power(PlayerCard, BotCard, ResultWinner),

    (ResultWinner =:= -1 -> WinnerCard = BotCard; WinnerCard = PlayerCard),
    update_score_of(ResultWinner, WinnerCard), !.

comparation_stage :-
    call_screen("comparacao"),

    get_last_cards(PlayerCard, BotCard),
    get_winner(PlayerCard, BotCard, ResultWinner),

    (ResultWinner =:= -1 -> WinnerCard = BotCard; WinnerCard = PlayerCard),
    update_score_of(ResultWinner, WinnerCard).

/*
 * Predicados que resolvem o loop após o fim de uma batalha.
 */
battle_win :-
    call_screen("vitoria"),
    campaign_stage.

battle_defeat :-
    call_screen("derrota"),
    campaign_stage.

battle_draw :-
    call_screen("empate"),
    campaign_stage.

/*
 * Define o estágio de finalização de uma campanha, isso por game clear.
 */
campaign_clear :-
    save_campaign_data,
    call_screen("gameClear").

/*
 * Define o estágio de finalização de uma campanha, isso por game end.
 */
campaign_end :-
    save_campaign_data,
    call_screen("gameOver").

% ==================== AUXILIARES ==================== %

/*
 * Inicia os parâmetros iniciais de uma batalha.
 */
build_battle :-
    update_screen_state("batalha"),
    build_deck(PlayerDeck),
    build_deck(BotDeck),
    init_battle_state(PlayerDeck, BotDeck).

/*
 * Constrói um deck de cartas permutada.
 */
build_deck(Deck) :-
    fill_deck(1, PureDeck),
    random_permutation(PureDeck, Deck).

/*
 * Predicado que simplifica a chamada da tela para o estágio atual.
 */
call_screen(NextScreen) :-
    update_screen_state(NextScreen),
    action,
    read_line(_).

/*
 * Predicado que simplifica a adição de uma nova campanha ao ranking.
 * 
 * Armazena, ao final de uma campanha, o nome e os pontos de um desafiante.
 */
save_campaign_data :-
    get_campaign_state(CampaignData),

    nth0(0, CampaignData, PlayerName),
    nth0(1, CampaignData, PlayerFinalScore),
    add_rank(PlayerName, PlayerFinalScore).

/*
 * Atualiza, se necessário, a vida do Player quando a pontuação for de 25.
 */
campaign_life_situation(PlayerData):-
    nth0(0, PlayerData, PlayerScore),
    (PlayerScore >= 25 -> update_player_life(1); update_player_life(0)).

/*
 * Pega a última carta jogada pelo jogador e a última carta jogada pelo bot.
 */
get_last_cards(PlayerLastCard, BotLastCard) :-
    get_player_state(PlayerData),
    get_bot_state(BotData),

    nth0(2, PlayerData, PlayerDeck),
    nth0(2, BotData, BotDeck),
    nth0(14, PlayerDeck, PlayerLastCard),
    nth0(14, BotDeck, BotLastCard).

/*
 * Lê a entrada do usuário de modo que não precise de um ponto no final da entrada.
 */
read_line(Result) :-
    read_line_to_codes(user_input, Codes),
    string_codes(Result, Codes).

/*
 * Valida se uma entrada do usuário está de acordo com as opções atuais.
 *
 * Recebe uma lista de opções válidas, uma string que representa a entrada do player
 * na situação atual e um parâmetro que representa a saída.
 */
validation_input(Options, Input, Result) :-
    string_chars(Input, InputToChar),
    contains_input(Options, InputToChar, OutContains),
    length(OutContains, Total),

    LastOut is Total - 1,
    (OutContains == [] -> Result = "";
    nth0(LastOut, OutContains, OutResult), Result = OutResult).

/*
 * Retorna as entradas que estão dentre as válidas para o loop atual.
 *
 * Recebe uma lista com as opções válidas, uma lista contendo os caracteres digitados
 * pelo jogador e o parâmetro de saída.
 */
contains_input(_, [], []) :- !.
contains_input(Options, [HInput | TInput], [Hresult | Tresult]) :-
    string_upper(HInput, InputCurrent),
    member(InputCurrent, Options),

    Hresult = InputCurrent,
    contains_input(Options, TInput, Tresult), !.

contains_input(Options, [_ | TInput], Result) :-
    contains_input(Options, TInput, Result).
