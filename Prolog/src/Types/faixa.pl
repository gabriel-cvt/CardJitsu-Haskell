:- module(faixa, [faixa/1, get_faixa/2, up_faixa/2]).

:- use_module('src/Types/player.pl').

faixa(branca).
faixa(azul).
faixa(roxa).
faixa(marrom).
faixa(preta).
faixa(mestre).

faixa_sucessora(branca, azul).
faixa_sucessora(azul, roxa).
faixa_sucessora(roxa, marrom).
faixa_sucessora(marrom, preta).
faixa_sucessora(preta, mestre).

get_faixa(player(_, Faixa, _), Faixa).

up_faixa(player(Nome, FaixaAtual, Progresso), player(Nome, NovaFaixa, Progresso)) :-
    faixa_sucessora(FaixaAtual, NovaFaixa).

faixas([branca, azul, roxa, marrom, preta, mestre]).

verificar_faixa :-
    salvamentos:carregar_jogador(Jogador),
    get_faixa(Jogador, FaixaAtual),

    faixa_anteriores(FaixaAtual, FaixasAnteriores),
    faixa_restantes(FaixaAtual, FaixasRestantes),

    exibir_faixa(FaixaAtual, FaixasAnteriores, FaixasRestantes).

exibir_faixa(branca, _, FaixasRestantes) :-
    write("Você ainda não alcançou outras faixas. Vamos começar a sua jornada ninja!\n"),
    write("Faixas anteriores:\n ➤ Nenhuma\n"),
    write("Faixas restantes:\n"),
    maplist(exibir_faixa_restante, FaixasRestantes).

exibir_faixa(preta, FaixasAnteriores, _) :-
    write("Parabéns! Você conquistou a faixa preta!\n"),
    write("Faixas anteriores:\n"),
    maplist(exibir_faixa_anterior, FaixasAnteriores),
    write("Objetivo restante:\n ➤ Desafiar o sensei e virar um Mestre ninja\n\n").

exibir_faixa(mestre, FaixasAnteriores, _) :-
    write("Parabéns! Você é um grande mestre Ninja!\n"),
    write("Faixas anteriores:\n"),
    maplist(exibir_faixa_anterior, FaixasAnteriores).

exibir_faixa(FaixaAtual, FaixasAnteriores, FaixasRestantes) :-
    format("Você está na faixa: ~s~n", [FaixaAtual]),
    write("Faixas anteriores:\n"),
    maplist(exibir_faixa_anterior, FaixasAnteriores),
    write("Faixas restantes:\n"),
    maplist(exibir_faixa_restante, FaixasRestantes).

exibir_faixa_anterior(Faixa) :-
    format(" ➤ ~s~n", [Faixa]).

exibir_faixa_restante(Faixa) :-
    format(" ➤ ~s~n", [Faixa]).

faixa_anteriores(FaixaAtual, FaixasAnteriores) :-
    faixas(Faixas),
    nth1(FaixaIndex, Faixas, FaixaAtual),
    findall(Faixa, (nth1(Index, Faixas, Faixa), Index < FaixaIndex), FaixasAnteriores).

faixa_restantes(FaixaAtual, FaixasRestantes) :-
    faixas(Faixas),
    nth1(FaixaIndex, Faixas, FaixaAtual),
    findall(Faixa, (nth1(Index, Faixas, Faixa), Index > FaixaIndex), FaixasRestantes).
