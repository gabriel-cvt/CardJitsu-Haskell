:- module(elemento, [elemento/1,
                     prioridade_elemento/2,
                     mostrar_elemento/2]).

elemento(fogo).
elemento(agua).
elemento(neve).

prioridade_elemento(fogo, neve).
prioridade_elemento(neve, agua).
prioridade_elemento(agua, fogo).
prioridade_elemento(_, _):- false.

mostrar_elemento(fogo, 'FOGO').
mostrar_elemento(agua, 'ÁGUA').
mostrar_elemento(neve, 'NEVE').


