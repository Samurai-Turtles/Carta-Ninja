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
elemInd(natureza, 1).
elemInd(agua, 2).
elemInd(metal, 3).
elemInd(terra, 4).

%Cria uma lista de tamanho Times do elemento Element passado como parâmetro
createList(Element, Times, Out):-
    Times > 0,
    T is Times - 1,
    createList(Element, T, Partial),
    append(Partial, [Element], Out), !.
createList(_, 0, []).

%A partir de uma lista de pesos, WList, recebida como parâmetro, cria uma lista com os elementos a serem
%sorteados como escolha do bot

%Não Funciona, está com algum erro na recursão que eu não consegui identificar ainda.
weigthHand(_, [], []).
weigthHand(WList, [H|Tail], Out):-
    getElem(H, E), %Funciona
    elemInd(E, I), %Funciona
    nth0(I, WList, W), %Funciona
    createList(H, W, List), %Funciona
    weigthHand(WList, Tail, Partial), %Não Funciona
    append(List, Partial, Final).

%Função main para teste e debug
main:-
    hand(H),
    % getElem(H, E),
    % elemInd(E, I),
    % nth0(I, [3, 2, 2, 2, 2], W),
    % createList(H, W, List), 
    weigthHand([3, 2, 2, 2, 2], H, O),
    % getElem(A, El),
    % elemInd(El, In),
    % nth0(In, [3,2,2,2,2], Ww),
    % createList(A, Ww, Partial), 
    % append(List, Partial, O),
    write(O),
    halt.