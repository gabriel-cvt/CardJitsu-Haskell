module(inimigo, [inimigo_jogar_carta/3]).

:- use_module('../Types/baralho.pl').

inimigo_jogar_carta(baralho([Carta|Resto]), Carta, Resto).