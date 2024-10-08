:- module(carta, [carta/2, 
                  mostrar_carta/2,
                  limitar_valor/2]).
                
:- use_module('elemento.pl').

carta(Elemento, Valor):-
    elemento(Elemento),
    integer(Valor).

mostrar_carta(carta(Elemento, Valor), StringCarta):-
    mostrar_elemento(Elemento, NomeElemento),
    number_string(Valor, ValorStr),
    format(atom(StringCarta), '(~w : ~s)', [NomeElemento, ValorStr]).

limitar_valor(Valor, 12):- Valor > 12.
limitar_valor(Valor, 1):- Valor < 1.
limitar_valor(Valor, Valor):- Valor >= 1, Valor =< 12.
