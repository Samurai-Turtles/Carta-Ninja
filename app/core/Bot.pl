:- consult('../util/StateManager.pl').

/* 
Pegar o deck do jogador:
get_player_state(D),
nth0(2,D,Deck).
*/

/*
Pegar a mao do bot:
get_bot_state(D)
nth0(2,D,Deck)
*/

/* 
Função que seleciona o index de uma das cartas da mão do bot
*/
makeChoice(Level,Index):-
    get_bot_state(D),
    nth0(2,D,Deck),
    slice(0, 5, Deck, Hand),
    wDeck(Level,Weight),
    wHand(Hand,Weight, W_cpu_hand),
    length(W_cpu_hand, L),
    random(0,L,Choice),
    nth0(Choice,W_cpu_hand,Id),
    nth0(Id,Hand,card(id(Index),elem(_),power(_))),

/*
Retorna os pesos de cada elemento na ordem FNAMT
*/
wDeck(Level,Weight):-
    get_player_state(D),
    nth0(2,D,Deck),
    V is 1+(6*Level),
    createList(V,5,CurrentWeight),
    wDeckRecursive(Deck, Level, CurrentWeight, Weight).

/*
Varre todo o deck, modificando os pesos de acordo com as cartas encontradas
*/
wDeckRecursive([],_,CurrentWeight,CurrentWeight):-!.
wDeckRecursive([card(id(_), elem('fire'), power(_))|T], Level, CurrentWeight, Weight):-
    updateWeight(I, [2,4,1,3], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.
wDeckRecursive([card(id(_), elem('nature'), power(_))|T], Level, CurrentWeight, Weight):-
    updateWeight(I, [0,3,2,4], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.
wDeckRecursive([card(id(_), elem('water'), power(_))|T], Level, CurrentWeight, Weight):-
    updateWeight(I, [1,4,0,3], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.
wDeckRecursive([card(id(_), elem('metal'), power(_))|T], Level, CurrentWeight, Weight):-
    updateWeight(I, [0,2,1,4], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.
wDeckRecursive([card(id(_), elem('earth'), power(_))|T], Level, CurrentWeight, Weight):-
    updateWeight(I, [1,3,0,2], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.

/*
Atualiza a lista de pesos
*/
updateWeight(Level, [Idx_to_increase1, Idx_to_increase2, Idx_to_decrease1, Idx_to_decrease2],CurrentWeight,NewWeight):-
    auxUpdateWeight(Level,Idx_to_increase1,CurrentWeight,New1),
    auxUpdateWeight(Level,Idx_to_increase2,New1,New2),
    NegativeI is Level * (-1),
    auxUpdateWeight(NegativeI,Idx_to_decrease1,New2,New3),
    auxUpdateWeight(NegativeI,Idx_to_decrease2,New3,NewWeight).

auxUpdateWeight(Level,Idx,Current,New):-
    nth0(Idx,Current,Increase),
    Value_to_increase is Increase + I,
    updateArray(Current, Idx, Value_to_increase, New).
    
/*
Atualiza o valor de uma lista na posição Idx por um Novo valor (NV)
*/
updateArray([_|T],0, NV, [NV|T]):-!.
updateArray([H|T],Idx, NV, [H|Return]):-
    NIdx is Idx - 1,
    updateArray(T,NIdx, NV, Return).

/*
Cria uma lista com um valor V repetido N vezes
*/
createList(_, 0, []). % Caso base: quando N é 0, a lista está vazia.
createList(V, N, [V|Resto]) :-
    N > 0, % Garante que N seja maior que 0 para continuar a recursão.
    N1 is N - 1, % Decrementa N para a próxima chamada recursiva.
    createList(V, N1, Resto). % Chama recursivamente para o restante da lista.

/*
Monta uma lista onde cada índice de 0 a 5 [0,5) é repetido um número de vezes igual
ao peso do elemento naquela posição na mão do bot
*/
wHand(Hand,WeightList,WeightHand):-
    wHandRecursive(0,Hand, WeightList, WeightHand).

wHandRecursive(5, _, _, []):-!.
wHandRecursive(I, [card(id(_),elem('fire'),power(_))|T], WeightList, Return):-
    nth0(0, WeightList, N),
    createList(I, N, List),
    Ni is I + 1 ,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).
wHandRecursive(I, [card(id(_),elem('nature'),power(_))|T], WeightList, Return):-
    nth0(1, WeightList, N),
    createList(I, N, List),
    Ni is I + 1 ,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).
wHandRecursive(I, [card(id(_),elem('water'),power(_))|T], WeightList, Return):-
    nth0(2, WeightList, N),
    createList(I, N, List),
    Ni is I + 1 ,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).
wHandRecursive(I, [card(id(_),elem('metal'),power(_))|T], WeightList, Return):-
    nth0(3, WeightList, N),
    createList(I, N, List),
    Ni is I + 1,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).
wHandRecursive(I, [card(id(_),elem('earth'),power(_))|T], WeightList, Return):-
    nth0(4, WeightList, N),
    createList(I, N, List),
    Ni is I + 1 ,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).

/*
Função slice auxiliar
*/
slice(0, End, [H|T], [H|Return]):-
    End > 0,
    NewEnd is End - 1,
    slice(0,NewEnd,T,Return),
    !.
slice(0,0,[H|_],[H]):-!.
slice(Start,End,[_|T],Return):-
    NewStart is Start - 1,
    slice(NewStart,End,T,Return).


