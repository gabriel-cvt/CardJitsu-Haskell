:- module(poder, [poder/1, 
                  mostrar_poder/2]).

:- use_module('elemento.pl').

poder(null).
poder(mais_dois).
poder(menos_dois).
poder(inverte).
poder(bloquear(Elemento)) :- elemento(Elemento).

mostrar_poder(mais_dois, 'Mais Dois').
mostrar_poder(menos_dois, 'Menos Dois').
mostrar_poder(inverte, 'Inverte').
mostrar_poder(bloquear(Elemento), StringPoder):-
    mostrar_elemento(Elemento, NomeElemento),
    format(atom(StringPoder), 'Bloquear [~w]', [NomeElemento]).

pega_poder_pilha([Poder|_],Poder).