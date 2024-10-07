:- module(deque, [deque/1,
                  novo_deque/3,
                  completar_deque/4,
                  jogar_carta/4,
                  mostrar_deque/2]).

:- use_module('carta.pl').
:- use_module('baralho.pl').

% Define um deque contendo cartas
deque([Carta]):- carta(Carta).

% Cria um novo deque extraindo 5 cartas do baralho
novo_deque(Baralho, deque(Deque), Resto):-
    extrair_cartas(5, Baralho, Deque, Resto).

% Completa o deque até ter 5 cartas
completar_deque(deque(Deque), _, deque(Deque), _) :- 
    length(Deque, Tamanho), 
    Tamanho >= 5.

completar_deque(deque(Cartas), Baralho, deque(Deque), Resto) :-
    length(Cartas, Tamanho),
    Tamanho < 5,
    pegar_carta(Baralho, Carta, NovoBaralho),
    completar_deque(deque([Carta|Cartas]), NovoBaralho, deque(Deque), Resto).

% Jogar Carta do Deque
jogar_carta(1, deque([Carta|Resto]), Carta, Resto).    
jogar_carta(Index, deque([Fst|Deque]), Carta, deque([Fst|Resto])):-
    Index \= 1,
    NewIdx is Index - 1,
    jogar_carta(NewIdx, deque(Deque), Carta, deque(Resto)).


% Exibe o conteúdo do deque
mostrar_deque(deque(Deque), StringDeque) :-
    mostrar_deque(deque(Deque), 1, StringDeque).

mostrar_deque(deque([]),_,"").
mostrar_deque(deque([Carta|Resto]), Index, StringDeque) :-
    mostrar_carta(Carta, StringCarta),
    
    format(atom(StringAtual), '[~d] ~w', [Index, StringCarta]),
    
    NewIdx is Index + 1,
    mostrar_deque(deque(Resto), NewIdx, StringResto),
    
    (StringResto = "" ->
        StringDeque = StringAtual
    ;
        format(atom(StringDeque), '~w\n~w', [StringAtual, StringResto])
    ).