% Este arquivo contém a lógica relativa ao gerenciamento de estados do jogo

/*
 * Inicializa o ScreenState, contendo a tela inicial do jogo.
 */
init_screen_state:- nb_setval(screen, "menu").

/*
 * Inicializa o CampaignState, contendo os dados da campanha atual.
 *
 * Os dados da campanha estão dispostos nas seguintes posições da lista:
 *
 *   1º) Name: nome da campanha (passado como parâmetro)
 *   2º) Pontuação total da campanha (inicia em 0)
 *   3º) Vidas extras do jogador (inicia em 2)
 *   4º) Nível de faixa (inicia em 1)
 */
init_campaign_state(Name):- nb_setval(campaign, [Name, 0, 2, 1]).

/*
 * Inicializa o BattleState, contendo os dados da batalha em andamento.
 *
 * Os decks do Jogador e do Bot são passados como parâmetro ao inicializar
 * o BattleState.
 *
 * Os dados da batalha estão dispostos nas seguintes posições da lista:
 *
 *   1º) Número do Round (inicia em 1, máximo 10)
 *   2º) Dados do Jogador: lista com pontuação, Streak, Deck e vitórias por elemento
 *   3º) Dados do Bot: lista com pontuação, Streak, Deck e vitórias por elemento
 *   4°) Dados Extras: uso de carta especial na rodada e as cartas especiais
 */
init_battle_state(PlayerDeck, BotDeck):-
    nb_setval(battle, [1, 
        [0, 0, [PlayerDeck], [false, false, false, false, false]], 
        [0, 0, [BotDeck], [false, false, false, false, false]],
        [false, ["swapInDeck","nullifyElement","swapBetweenHands"]]]).

/*
 * Atualiza o ScreenState atual, guardando o ID da nova tela.
 */
update_screen_state(Screen):- nb_setval(screen, Screen).

/*
 * Atualiza o CampaignState, guardando a pontuação total, número de vidas e 
 * nível de faixa.
 */
update_campaign_state(Pts, Lifes, Belt):- 
    nb_getval(campaign, State),
    nth0(0, State, Name),
    nb_setval(campaign, [Name, Pts, Lifes, Belt]).

/*
 * Atualiza o Round no BattleState, guardando o novo valor do Round.
 */
update_round_state(NewRound):-
    nb_getval(battle, State),
    nth0(1, State, PlayerData),
    nth0(2, State, BotData),
    nth0(3, State, ExtraData),
    nb_setval(battle, [NewRound, PlayerData, BotData, ExtraData]).

/*
 * Atualiza os dados do Jogador no BattleState, guardando a nova lista de dados.
 */
update_player_state(NewPlayerData):-
    nb_getval(battle, State),
    nth0(0, State, Round),
    nth0(2, State, BotData),
    nth0(3, State, ExtraData),
    nb_setval(battle, [Round, NewPlayerData, BotData, ExtraData]).

/*
 * Atualiza os dados do Bot no BattleState, guardando a nova lista de dados.
 */
update_bot_state(NewBotData):-
    nb_getval(battle, State),
    nth0(0, State, Round),
    nth0(1, State, PlayerData),
    nth0(3, State, ExtraData),
    nb_setval(battle, [Round, PlayerData, NewBotData, ExtraData]).

/*
 * Atualiza os dados do extras no BattleState, guardando a nova lista de dados.
 */
update_extra_state(NewExtraData):-
    nb_getval(battle, State),
    nth0(0, State, Round),
    nth0(1, State, PlayerData),
    nth0(2, State, BotData),
    nb_setval(battle, [Round, PlayerData, BotData, NewExtraData]).

/*
 * Retorna o ScreenState atual.
 */
get_screen_state(State):- nb_getval(screen, R), State = R.

/*
 * Retorna o CampaignState atual.
 */
get_campaign_state(State):- nb_getval(campaign, R), State = R.

/*
 * Retorna o valor atual do Round.
 */
get_current_round(Value):-
    nb_getval(battle, State),
    nth0(0, State, Round),
    Value = Round.

/*
 * Retorna a lista de dados do Jogador.
 *
 * Os dados estão dispostos nas seguintes posições:
 *
 *   1º) Pontuação da partida
 *   2º) Streak de vitórias
 *   3º) Deck de cartas
 *   4º) Vitórias por elemento
 */
get_player_state(PlayerData):-
    nb_getval(battle, State),
    nth0(1, State, Data),
    PlayerData = Data.

/*
 * Retorna a lista de dados do Bot.
 *
 * Os dados estão dispostos nas seguintes posições:
 *
 *   1º) Pontuação da partida
 *   2º) Streak de vitórias
 *   3º) Deck de cartas
 *   4º) Vitórias por elemento
 */
get_bot_state(BotData):-
    nb_getval(battle, State),
    nth0(2, State, Data),
    BotData = Data.

/*
 * Retorna a lista dos dados extra da batalha.
 *
 * Os dados estão dispostos nas seguintes posições:
 *
 *   1º) Uso de uma carta especial na rodada atual.
 *   2º) Deck das cartas especiais.
 */
get_extra_state(ExtraData):-
    nb_getval(battle, State),
    nth0(3, State, Data),
    ExtraData = Data.