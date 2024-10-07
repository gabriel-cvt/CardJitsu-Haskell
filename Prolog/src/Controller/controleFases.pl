:- module(controle_fase, [fase_jogador/1]).

:- use_module('./src/Util/lib.pl').
:- use_module('./src/Controller/controleJogo.pl').
:- use_module('./src/Controller/controleCarregamento.pl').
:- use_module('./src/Services/saidas.pl').

fase_jogador(branca) :-
    lib:clearScreen,
    saidas:texto(branca),
    lib:pressionar_tecla,
    controle_jogo:start_partida("O Ruivo").

fase_jogador(azul) :-
    lib:clearScreen,
    saidas:texto(azul),
    lib:pressionar_tecla,
    controle_jogo:start_partida("Bruxa da Neve").

fase_jogador(roxa) :-
    lib:clearScreen,
    saidas:texto(roxa),
    lib:pressionar_tecla,
    controle_jogo:start_partida("Caveleiro Do Mar").

fase_jogador(marrom) :-
    lib:clearScreen,
    saidas:texto(marrom),
    lib:pressionar_tecla,
    controle_jogo:start_partida("Punhos de Fogo").

fase_jogador(preta) :-
    lib:clearScreen,
    saidas:texto(preta),
    lib:pressionar_tecla,
    controle_jogo:start_partida("Olhos de Falcão").

fase_jogador(mestre) :-
    lib:clearScreen,
    saidas:texto(final),
    lib:pressionar_tecla,
    saidas:texto(zerou),
    escolha_final(Escolha).


escolha_final(1) :-
    lib:clearScreen,
    saidas:centraliza("Então você vai abandonar tudo que conseguiu e ir atrás da conquista novamente...\n"),
    lib:pressionar_tecla,
    controle_carregamento:novo_jogo.

escolha_final(2) :-
    saidas:centraliza("Bem, o caminho que você fez não foi fácil.\nApós tantas batalhas, chegou o momento de descansar\n"),
    lib:pressionar_tecla,
    halt(0).

escolha_final(_) :-
    saidas:centraliza("Pelo visto a jornada foi tão desafiadora que confundiu suas teclas...\nEscolha novamente!\n"),
    lib:pressionar_tecla
    controle_carregamento:fase_jogador(mestre).