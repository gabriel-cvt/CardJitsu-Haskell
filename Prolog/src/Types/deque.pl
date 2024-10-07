:- module(deque, [deque/1,
                  novo_deque/3,
                  completar_deque/4,
                  jogar_carta/4,
                  mostrar_deque/2,
                  mostrar_baralho/2]).

:- use_module('carta.pl').
:- use_module('baralho.pl').

deque(Cartas):- is_list(Cartas).

novo_deque(Baralho, deque(Deque), Resto):-
    extrair_cartas(5, Baralho, Deque, Resto).

completar_deque(deque(Deque), _, deque(Deque), _):- 
    length(Deque, Tamanho), 
    Tamanho >= 5, !.

completar_deque(deque(Cartas), Baralho, deque(NovoDeque), RestoBaralho):- 
    length(Cartas, Tamanho),
    Tamanho < 5,
    (length(Baralho, BaralhoTamanho), BaralhoTamanho > 0 ->
        pegar_carta(Baralho, Carta, NovoBaralho),
        completar_deque(deque([Carta|Cartas]), NovoBaralho, deque(NovoDeque), RestoBaralho)
    ;
        NovoDeque = deque(Cartas),
        RestoBaralho = Baralho
    ).

jogar_carta(1, deque([Carta|Resto]), Carta, deque(Resto)).

jogar_carta(Index, deque([Fst|Deque]), Carta, deque([Fst|Resto])):- 
    Index > 1,
    NewIdx is Index - 1,
    jogar_carta(NewIdx, deque(Deque), Carta, deque(Resto)).
    
jogar_carta(Index, _, _, _).

mostrar_deque(deque(Deque), StringDeque):-
    mostrar_deque(deque(Deque), 1, StringDeque).

mostrar_deque(deque([]), _, "").
mostrar_deque(deque([Carta|Resto]), Index, StringDeque):-
    mostrar_carta(Carta, StringCarta),
    format(atom(StringAtual), '[~d] ~w', [Index, StringCarta]),
    NewIdx is Index + 1,
    mostrar_deque(deque(Resto), NewIdx, StringResto),
    (StringResto = "" -> 
        StringDeque = StringAtual 
    ; 
        format(atom(StringDeque), '~w\n~w', [StringAtual, StringResto])
    ).