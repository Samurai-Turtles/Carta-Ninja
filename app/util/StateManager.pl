% Este arquivo contém a lógica relativa ao gerenciamento de estados do jogo

/*
 * Predicado para inicializar o GlobalState.
 *
 * O GlobalState armazena a tela exibida ao jogador no momento.
 */
init_global_state:- nb_setval(screen, "menu").

/*
 * Predicado para inicializar o CampaignState.
 *
 * O CampaignState armazena uma lista com os seguintes valores:
 *   - Nome da campanha
 *   - Pontuação total (inicia em 0)
 *   - Vidas do jogador (inicia em 2)
 *   - Nível de faixa (inicia em 1)
 */
init_campaign_state:- nb_setval(campaign, ["", 0, 2, 1]).

/*
 * Predicado para inicializar o BattleState.
 *
 * O BattleState armazena uma lista com os seguintes valores:
 *   - Round atual
 *   - Dados do jogador (pontuação, streak, vitórias por elemento e deck)
 *   - Dados do bot (pontuação, streak, vitórias por elemento e deck)
 */
init_battle_state:- nb_setval(battle, [1, [1, 0, [], []], [1, 0, [], []]]).

test_states:-
    init_global_state, 
    init_campaign_state, 
    init_battle_state,
    nb_getval(screen, X),
    nb_getval(campaign, Y),
    nb_getval(battle, Z),
    write(X), nl,
    write(Y), nl,
    write(Z).
