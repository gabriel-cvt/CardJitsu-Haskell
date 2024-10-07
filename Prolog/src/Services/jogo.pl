:- module(jogo, [iniciar_jogo/2]).

% Apenas testando o Controler Jogo

% Pro retorno ser booleano mesmo, precisa colocar "!."
% Se não, retorna com endereço de memória junto

iniciar_jogo([0, 0], Vencedor):- 
    Vencedor = true, !.

iniciar_jogo([1,0], Vencedor) :- 
    Vencedor = false, !.

iniciar_jogo([0, 1], Vencedor) :- 
    Vencedor = false, !.

iniciar_jogo([1,1], Vencedor) :-
    Vencedor = false, !.