:- module(carta, [carta/3, 
                  mostrar_carta/2]).
                
:- use_module('elemento.pl').
:- use_module('poder.pl').

carta(Elemento, Valor, Poder):-
    elemento(Elemento),
    integer(Valor),
    poder(Poder).

mostrar_carta(carta(Elemento, Valor, Poder), StringCarta):-
    mostrar_elemento(Elemento, NomeElemento),
    number_string(Valor, ValorStr),
    format(atom(Base), '(~w : ~s)', [NomeElemento, ValorStr]),
    (Poder = null ->
        StringCarta = Base
    ;
        mostrar_poder(Poder, NomePoder),
        format(atom(StringCarta), '~s -> ~w', [Base, NomePoder])
    ).

limitar_valor(Valor, 12):- Valor > 12.
limitar_valor(Valor, 1):- Valor < 1.
limitar_valor(Valor, Valor):- Valor >= 1, Valor =< 12.
