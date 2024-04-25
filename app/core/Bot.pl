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

%Retorna o elemento de uma carta recebida como parâmetro
getElem(card(id(_), elem(E), power(_)), E).

%Retorna o poder de uma carta recebida como parâmetro
getPower(card(id(_), elem(_), power(P)), P).