:- module(salvamentos, [salvar_jogador/1, carregar_jogador/1, existe_progresso/1, carregar_instrucoes/1, up_jogador_partida/1]).

:- use_module(library(readutil)).

:- use_module('./src/Types/player.pl').
:- use_module('./src/Types/faixa.pl').

caminho_jogador('./src/Repositories/jogador.txt').

salvar_jogador(Jogador) :-
    caminho_jogador(Caminho),
    open(Caminho, write, Stream),
    write(Stream, Jogador),
    write(Stream, '.'),
    nl(Stream),
    close(Stream).

carregar_jogador(Jogador) :-
    caminho_jogador(Caminho),
    open(Caminho, read, Stream),
    read(Stream, Jogador),
    close(Stream).

existe_progresso(player(_, _, Progresso)) :-
    Progresso \= 0.

carregar_instrucoes(Texto) :- 
    open('./src/Repositories/instrucoes.txt', read, Stream),
    read_lines(Stream, Texto),
    close(Stream).

% Regra auxiliar para ler todas as linhas do arquivo
read_lines(Stream, []) :- 
    at_end_of_stream(Stream), !.

read_lines(Stream, [Linha|Linhas]) :- 
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, LinhaString),
    Linha = LinhaString,
    read_lines(Stream, Linhas).

up_jogador_partida(JogadorAtualizado) :-
    carregar_jogador(Jogador),
    faixa:up_faixa(Jogador, JogadorProxFaixa),
    player:up_progresso(JogadorProxFaixa, JogadorAtualizado),
    salvamentos:salvar_jogador(JogadorAtualizado).