:- module(baralho, [novo_baralho/1,
                    gerar_cartas_elemento/2,
                    pegar_carta/3,
                    extrair_cartas/4,
                    embaralhar/2,
                    mostrar_baralho/2]).

:- use_module(library(random)).
:- use_module('elemento.pl').
:- use_module('carta.pl').

gerar_cartas_elemento(Elemento, Cartas):-
    gerar_cartas_elemento(Elemento, 1, Cartas).

gerar_cartas_elemento(_, Valor, []):- Valor > 12.
gerar_cartas_elemento(Elemento, Valor, [Carta | Cartas]):-
    Valor =< 12,
    Carta = carta(Elemento, Valor),
    ProxValor is Valor + 1,
    gerar_cartas_elemento(Elemento, ProxValor, Cartas).

novo_baralho(Baralho):- 
    gerar_cartas_elemento(fogo, CartasFogo),
    gerar_cartas_elemento(agua, CartasAgua),
    gerar_cartas_elemento(neve, CartasNeve),
    append(CartasFogo, CartasAgua, FogoAgua),
    append(FogoAgua, CartasNeve, BaralhoTemp),
    embaralhar(BaralhoTemp, Baralho).

pegar_carta([], none, []).
pegar_carta([Carta|Cartas], Carta, Cartas).

extrair_cartas(0, Baralho, [], Baralho).
extrair_cartas(Qtd, Baralho, [Carta|Cartas], Resto):-
    Qtd > 0,
    pegar_carta(Baralho, Carta, RestoBaralho),
    NextQtd is Qtd - 1,
    extrair_cartas(NextQtd, RestoBaralho, Cartas, Resto).

embaralhar(Baralho, BaralhoEmbaralhado):-
    random_permutation(Baralho, BaralhoEmbaralhado).

mostrar_baralho([], "Baralho vazio.").
mostrar_baralho([Carta|Resto], StringBaralho):-
    mostrar_carta(Carta, StringCarta),
    mostrar_baralho(Resto, StringResto),
    format(atom(StringBaralho), '~w\n~w', [StringCarta, StringResto]).
