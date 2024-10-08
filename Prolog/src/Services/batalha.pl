:- module(batalha, [combate/3,
                    combate_valor/3]).

:- use_module('../Types/carta.pl').
:- use_module('../Types/elemento.pl').

combate(carta(Elemento,Valor1), carta(Elemento,Valor2), Ganhador):-
    combate_valor(Valor1, Valor2, Ganhador).

combate(carta(EleJogador,_), carta(EleBot,_), 1):-
    prioridade_elemento(EleJogador, EleBot).
combate(carta(EleJogador,_), carta(EleBot,_), 2):-
    prioridade_elemento(EleBot, EleJogador).

combate_valor(Valor, Valor, 0).
combate_valor(Valor1, Valor2, 1):- Valor1 > Valor2.
combate_valor(Valor1, Valor2, 2):- Valor2 > Valor1.
