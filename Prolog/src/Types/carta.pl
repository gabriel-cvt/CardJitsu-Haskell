:- module(carta, [poder/1, 
                  mostrar_poder/2, 
                  carta/3, 
                  mostrar_carta/4]).
                
:- use_module('./src/Types/elemento.pl').

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
    format(atom(StringPoder), 'Bloquear [~w]', [NomeElemento]),

carta(Elemento, Valor, Poder):-
    elemento(Elemento),
    integer(Valor),
    poder(Poder).

mostrar_carta(Elemento, Valor, Poder, StringCarta):-
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
