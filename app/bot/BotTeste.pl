:- consult('../util/StateManager.pl').

% Deletar esse arquivo somente após rodar o jogo pela primeira vez e ver que funcionou

/* 
Pegar o deck do jogador:
get_player_state(D),
nth0(2,D,Deck).
*/
deck([card(id(5), elem('metal'), power(5)), 
    card(id(4), elem('metal'), power(2)), 
    card(id(3), elem('fire'), power(8)), 
    card(id(2), elem('fire'), power(5)), 
    card(id(1), elem('fire'), power(2)),
    card(id(6), elem('metal'), power(8)),
    card(id(7), elem('nature'), power(2)), 
    card(id(8), elem('nature'), power(5)),
    card(id(9), elem('nature'), power(8)),
    card(id(10), elem('earth'), power(2))]).

/*
Pegar a mao do bot:
get_bot_state(D)
nth0(2,D,Deck)
*/
hand([card(id(5), elem('metal'), power(5)),
    card(id(3), elem('fire'), power(8)),
    card(id(7), elem('nature'), power(2)),
    card(id(10), elem('earth'), power(2)),
    card(id(13), elem('water'), power(2))]).

/* 
Função que seleciona o index de uma das cartas da mão do bot
*/
makeChoice(BotDeck,PlayerDeck,I,Index):-
    wDeck(PlayerDeck,I,Weight),
    %writeln('Weight:'),
    %writeln(Weight),
    wHand(BotDeck,Weight, W_cpu_hand),
    %writeln('WHand:'),
    %writeln(W_cpu_hand),
    length(W_cpu_hand, L),
    %writeln('L:'),
    %writeln(L),
    random(0,L,Choice),
    %writeln('Choice:'),
    %writeln(Choice),
    nth0(Choice,W_cpu_hand,Id),
    nth0(Id,BotDeck,Card),
    %writeln('Card:'),
    %writeln(Card),
    getIdx(Card,Index).    

getIdx(card(id(Index),elem(_),power(_)), Index).

/*
Retorna os pesos de cada elemento na ordem FNAMT
*/
wDeck(Deck,I,Weight):-
    V is 1+(6*I),
    repeat(V,5,CurrentWeight),
    wDeckRecursive(Deck, I, CurrentWeight, Weight).

/*
Varre todo o deck, modificando os pesos de acordo com as cartas encontradas
*/
wDeckRecursive([],_,CurrentWeight,CurrentWeight):-!.
wDeckRecursive([card(id(_), elem('fire'), power(_))|T], I, CurrentWeight, Weight):-
    updateWeight(I, [2,4,1,3], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.
wDeckRecursive([card(id(_), elem('nature'), power(_))|T], I, CurrentWeight, Weight):-
    updateWeight(I, [0,3,2,4], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.
wDeckRecursive([card(id(_), elem('water'), power(_))|T], I, CurrentWeight, Weight):-
    updateWeight(I, [1,4,0,3], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.
wDeckRecursive([card(id(_), elem('metal'), power(_))|T], I, CurrentWeight, Weight):-
    updateWeight(I, [0,2,1,4], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.
wDeckRecursive([card(id(_), elem('earth'), power(_))|T], I, CurrentWeight, Weight):-
    updateWeight(I, [1,3,0,2], CurrentWeight,NCW),
    wDeckRecursive(T,I,NCW,Weight),
    !.

/*
Atualiza a lista de pesos
*/
updateWeight(I, [Idx_to_increase1, Idx_to_increase2, Idx_to_decrease1, Idx_to_decrease2],CurrentWeight,NewWeight):-
    auxUpdateWeight(I,Idx_to_increase1,CurrentWeight,New1),
    auxUpdateWeight(I,Idx_to_increase2,New1,New2),
    NegativeI is I * (-1),
    auxUpdateWeight(NegativeI,Idx_to_decrease1,New2,New3),
    auxUpdateWeight(NegativeI,Idx_to_decrease2,New3,NewWeight).

auxUpdateWeight(I,Idx,Current,New):-
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
repeat(_, 0, []). % Caso base: quando N é 0, a lista está vazia.
repeat(V, N, [V|Resto]) :-
    N > 0, % Garante que N seja maior que 0 para continuar a recursão.
    N1 is N - 1, % Decrementa N para a próxima chamada recursiva.
    repeat(V, N1, Resto). % Chama recursivamente para o restante da lista.

/*
Monta uma lista onde cada índice de 0 a 5 [0,5) é repetido um número de vezes igual
ao peso do elemento naquela posição na mão do bot
*/
wHand(Hand,WeightList,WeightHand):-
    wHandRecursive(0,Hand, WeightList, WeightHand).

wHandRecursive(5, _, _, []):-!.
wHandRecursive(I, [card(id(_),elem('fire'),power(_))|T], WeightList, Return):-
    nth0(0, WeightList, N),
    repeat(I, N, List),
    Ni is I + 1 ,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).
wHandRecursive(I, [card(id(_),elem('nature'),power(_))|T], WeightList, Return):-
    nth0(1, WeightList, N),
    repeat(I, N, List),
    Ni is I + 1 ,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).
wHandRecursive(I, [card(id(_),elem('water'),power(_))|T], WeightList, Return):-
    nth0(2, WeightList, N),
    repeat(I, N, List),
    Ni is I + 1,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).
wHandRecursive(I, [card(id(_),elem('metal'),power(_))|T], WeightList, Return):-
    nth0(3, WeightList, N),
    repeat(I, N, List),
    Ni is I + 1 ,
    wHandRecursive(Ni, T, WeightList, WeightHand),
    append(List,WeightHand,Return).
wHandRecursive(I, [card(id(_),elem('earth'),power(_))|T], WeightList, Return):-
    nth0(4, WeightList, N),
    repeat(I, N, List),
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

main:-
    hand(H),
    %writeln('H:'),
    %writeln(H),
    deck(D),
    %writeln('D:'),
    %writeln(D),
    makeChoice(H,D,1,Index),
    %writeln('Index:'),
    %writeln(Index),
    halt.

