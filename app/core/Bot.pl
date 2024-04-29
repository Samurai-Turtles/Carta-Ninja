:- consult('../util/StateManager.pl').

%O fato deck apenas serve para teste, será excluido na versão final.
deck([card(id(5), elem('metal'), power(5)), 
    card(id(4), elem('metal'), power(2)), 
    card(id(3), elem('fire'), power(8)), 
    card(id(2), elem('fire'), power(5)), 
    card(id(1), elem('fire'), power(2)),
    card(id(6), elem('metal'), power(8)),
    card(id(7), elem('nature'), power(2)), 
    card(id(8), elem('nature'), power(5)),
    card(id(9), elem('nature'), power(8)),
    card(id(10), elem('earth'), power(2)),
    card(id(11), elem('earth'), power(5)), 
    card(id(12), elem('earth'), power(8)),
    card(id(13), elem('water'), power(2)), 
    card(id(14), elem('water'), power(5)),
    card(id(15), elem('water'), power(8))]).

%O fato hand apenas serve para teste, será excluido na versão final.
hand([card(id(5), elem('metal'), power(5)),
    card(id(3), elem('fire'), power(8)),
    card(id(7), elem('nature'), power(2)),
    card(id(10), elem('earth'), power(2)),
    card(id(13), elem('water'), power(2))]).


%Retorna o elemento de uma carta recebida como parâmetro
getElem(card(id(_), elem(E), power(_)), E).

%Retorna o poder de uma carta recebida como parâmetro
getPower(card(id(_), elem(_), power(P)), P).

%Fatos que contém as posições da lista de pesos
elemInd(fire, 0).
elemInd(nature, 1).
elemInd(water, 2).
elemInd(metal, 3).
elemInd(earth, 4).

%Cria uma lista de tamanho Times do elemento Element passado como parâmetro
createList(_, 0, []):-!.
createList(Element, Times, Out):-
    Times > 0,
    T is Times - 1,
    createList(Element, T, Partial),
    append(Partial, [Element], Out).

%A partir de uma lista de pesos, WList, recebida como parâmetro, cria uma lista com os elementos a serem
%sorteados como escolha do bot

weightHand(_, [], []):-!.
weightHand(WList, [H|Tail], Out):-
    getElem(H, E), %Funciona
    elemInd(E, I), %Funciona
    nth0(I,WList,W), %Funciona
    createList(H, W, List), %Funciona
    weightHand(WList, Tail, Partial),
    append(List, Partial, Final),
    writeln(Final).

%Cria a lista de pesos se baseando num valor I que define a dificuldade do bot
createWeightList(_,X,[6,6,6,6,6]):-
    X =< 1,
    !.
createWeightList(Deck,I,Return):-
    InitialV is 6 * I,
    repeat(InitialV, 5, StWL),
    %get_player_state(D),
    %nth0(2,D,Deck),
    countElem(Deck,[0,0,0,0,0],Count),
    fixWeightList(I,StWL,0,Count,Return).

% Monta a lista de pesos, removendo valores de uma elemento desfavorável e aumentando de um elemento favorável
fixWeightList(_,CurrentList, 5, _, CurrentList):-!.
fixWeightList(Multiplier,CurrentList, I, Count, Return):-
    nth0(I,CurrentList,CI),
    FL is ((I + 1) mod 5), % define o primeiro elemento que perde para o da posicao I (First Loser)
    nth0(FL,Count,NFL),
    SL is ((I + 3) mod 5), % define o segundo elemento que perde para o da posicao I  (Second Loser)
    nth0(SL,Count,NSL),
    NL is CI + (NFL + NSL)*Multiplier,
    FW is ((I+2) mod 5), % First Winner
    nth0(FW,Count,NFW),
    SW is ((I+4) mod 5),
    nth0(SW,Count,NSW),
    NW is NL - (NFW + NSW)*Multiplier,
    replace(I,NW,CurrentList,NewCurrent1),
    NI is I + 1,
    fixWeightList(Multiplier,NewCurrent1,NI,Count,Return).

% Cria uma lista com um valor V repetido N vezes
repeat(_, 0, []). % Caso base: quando N é 0, a lista está vazia.
repeat(V, N, [V|Resto]) :-
    N > 0, % Garante que N seja maior que 0 para continuar a recursão.
    N1 is N - 1, % Decrementa N para a próxima chamada recursiva.
    repeat(V, N1, Resto). % Chama recursivamente para o restante da lista.

% Retorna uma lista com o número de vezes que cada elemento aparece
countElem([],Count,Count):-!.
countElem([Card|T],Count,Return):-
    getElem(C,E),
    elemInd(E,I),
    nth0(I,Count,R),
    NR is R + 1,
    replace(I,NR,Count,NewCount),
    countElem(T,NewCount,Return).

% Troca o elemento na posição I pelo elemento E da lista passada
replace(0,E,[H|T],[E|T]):-!.
replace(I,E,[H|T],[H|NR]):-
    NI is I-1,
    replace(NI,E,T,NR).

%Dado um valor I e a mão atual do bot, seleciona uma carta
%A seleção é baseada na lista dos pesos das cartas
makeChoice(Deck,I,Hand, Card):-
    createWeightList(Deck,I,WList),
    min_list(WList,Min),
    abs(Min,M),
    V is M + 1,
    sumToAll(WList,V,NWList),
    writeln(NWList),
    weightHand(NWlist, Hand, Options),
    length(Hand, L), %Substituir por Options quando weightHand estiver funcionando
    random(0, L, Index),
    nth0(Index, Options, Card).

sumToAll([],_,[]).
sumToAll([H|T],V,[NH|R]):-
    NH is H + V,
    sumToAll(T,V,R).

abs(N,N):-
    N >= 0,
    !.
abs(N,R):-
    R is N * (-1).

%Função main para teste e debug
main:-
    hand(H),
    deck(D),
    makeChoice(D,2,H,Card),
    writeln(Card),
    halt.