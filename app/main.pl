% Este é o arquivo principal do projeto
% A partir daqui, o usuário iniciará o jogo
:- consult(['./core/GameLoop.pl', './util/StateManager.pl']).
%:- initialization(main).

main :-
    init_loop.
