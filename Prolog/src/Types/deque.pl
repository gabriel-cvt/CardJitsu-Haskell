:- module(deque, [deque/1,
                  novo_deque/3,
                  completar_deque/4,
                  jogar_carta/2]).

:- use_module('./src/Types/carta.pl').
:- use_module('./src/Types/baralho.pl').

deque([Carta]):- carta(Carta).

% Regra responsável por criar um Deque
% extrai 5 cartas do baralho que são
% passadas para o deque.
novo_deque(Baralho, Deque, Resto):-
    extrair_cartas(5, Baralho, Deque, Resto).

% Regra que recebe um Deque e um Baralho,
% completa as cartas do Deque
% e retorna o Baralho sem as cartas retiradas.
completar_deque(Deque, _, Deque, _):- 
    length(Deque, Tamanho), 
    Tamanho >= 5.

completar_deque(Cartas, Baralho, Deque, Resto):-
    length(Cartas, Tamanho),
    Tamanho < 5,
    pegar_carta(Baralho, Carta, NovoBaralho),
    completar_deque([Carta|Cartas], NovoBaralho, Deque, Resto).

% Fazer Jogar Carta!
