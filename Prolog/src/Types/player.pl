:- module(player, [novo_jogador/2, get_progresso/2, up_partidas_jogadas/2, get_nome/2, get_faixa/2]).

:- [Types_faixa].

jogador :- Player(Nome, Faixa, Partidas).

get_nome(player(Nome, _, _), Nome).

get_faixa(player(_, Faixa, _), Faixa).

get_progresso(player(_, _, Progresso), Progresso).

up_progresso(player(Nome, Faixa, Progresso), player(Nome, Faixa, NovoProgreso)) :-
    NovoProgreso is Progresso + 1.

novo_jogador(Nome, player(Nome, branca, 1)).
