:- module(jogo, [iniciar_jogo/3]).

:- use_module('../Util/lib.pl').
:- use_module('../Types/elemento.pl').
:- use_module('../Types/carta.pl').
:- use_module('../Types/baralho.pl').
:- use_module('../Types/deque.pl').
:- use_module('batalha.pl').
:- use_module('inimigo.pl').
:- use_module('saidas.pl').

:- dynamic estado_jogo/10.

estado_jogo(0,0, [], [], [], [], [], [], [], []).

iniciar_jogo(Jogador, Inimigo, Vencedor) :-
    baralho:novo_baralho(TempJogador),
    baralho:novo_baralho(TempInimigo),
    deque:novo_deque(TempJogador, DequeJogador, BaralhoJogador),
    deque:novo_deque(TempInimigo, DequeInimigo, BaralhoInimigo),
    
    assertz(estado_jogo(Jogador, Inimigo,
                        BaralhoJogador,
                        BaralhoInimigo,
                        DequeJogador,
                        DequeInimigo,
                        [[], [], []],
                        [[], [], []],
                        [], [])),

    loop_jogo(Vencedor).

loop_jogo(Vencedor) :-
    retract(estado_jogo(Jogador, Inimigo, BaralhoJogador, BaralhoInimigo, DequeJogador, DequeInimigo, ElementosJogador, ElementosInimigo, PilhaJogador, PilhaInimigo)),
    
    print_estado_jogo,

    saidas:centraliza_format([bold, fg(blue)], "Escolha uma carta para jogar [1-5]:"),
    input_to_number(Index),
    Index > 0, Index < 6,
    
    deque:jogar_carta(Index, DequeJogador, CartaJogador, DequeJogador2),
    deque:jogar_carta(1,DequeInimigo,CartaInimigo,DequeInimigo2),
   % inimigo:inimigo_jogar_carta(BaralhoInimigo, CartaInimigo, BaralhoInimigo2),
    deque:completar_deque(DequeJogador2,BaralhoJogador,NewDequeJogador,NewBaralhoJogador),
    deque:completar_deque(DequeInimigo2,BaralhoInimigo,NewDequeInimigo,NewBaralhoInimigo),
    CartaJogador = carta(ElementoJogador, _, PoderJogador),
    CartaInimigo = carta(ElementoInimigo, _, PoderInimigo),
    pega_poder_pilha(PilhaJogador, PJ),
    pega_poder_pilha(PilhaInimigo, PI),
    combate(CartaJogador, CartaInimigo, PJ, PI, Ganhador),
    (Ganhador = 1 ->
        (ElementoJogador = fogo ->
            NovoElementosJogador = [[fogo|ElementosJogador], _, _]
        ; ElementoJogador = agua ->
            NovoElementosJogador = [_, [agua|ElementosJogador], _]
        ; ElementoJogador = neve ->
            NovoElementosJogador = [_, _, [neve|ElementosJogador]]
        ),
        (checa_vencedor(NovoElementosJogador) -> Vencedor is 1, 
            saidas:centraliza("Fim do jogo, jogador venceu!")
        ; 
        assertz(estado_jogo(Jogador, Inimigo, NewBaralhoJogador, NewBaralhoInimigo, NewDequeJogador, NewDequeInimigo, NovoElementosJogador, ElementosInimigo, [PoderJogador|PilhaJogador], [PoderInimigo|PilhaInimigo])),
        loop_jogo(Vencedor)
        )


    ; Ganhador = 2 ->
        (ElementoInimigo = fogo ->
            NovoElementosInimigo = [[fogo|ElementosInimigo], _, _]
        ; ElementoInimigo = agua ->
            NovoElementosInimigo = [_, [agua|ElementosInimigo], _]
        ; ElementoInimigo = neve ->
            NovoElementosInimigo = [_, _, [neve|ElementosInimigo]]
        ),
        (checa_vencedor(NovoElementosInimigo) -> Vencedor is 2
        ; 
        assertz(estado_jogo(Jogador, Inimigo, NewBaralhoJogador, NewBaralhoInimigo, NewDequeJogador, NewDequeInimigo, ElementosJogador, NovoElementosInimigo, [PoderJogador|PilhaJogador], [PoderInimigo|PilhaInimigo])),
        loop_jogo(Vencedor)
        )
    ).
checa_vencedor([Fogo, Agua, Neve]) :-
    (length(Fogo, 1), length(Agua, 1), length(Neve, 1))
    ;
    length(Fogo, 3)
    ;
    length(Agua, 3)
    ;
    length(Neve, 3).



print_estado_jogo :-
    retract(estado_jogo(Jogador, Inimigo,
                        BaralhoJogador, BaralhoInimigo,
                        DequeJogador, DequeInimigo,
                        ElementosJogador, ElementosInimigo,
                        PilhaJogador, PilhaInimigo)),
    lib:clearScreen,
    exibir_placar(Jogador, Inimigo),
    exibir_elementos(ElementoJogador, ElementoInimigo),
    print_poderes_ativos(PilhaJogador, PilhaInimigo), nl.

exibir_elementos(ElementosJogador, ElementosInimigo) :-
    saidas:centraliza_format([bold, fg(green)], "Seus elementos acumulados:"), nl,
    print_elementos_acumulados(ElementosJogador), nl, nl,
    saidas:centraliza_format([bold, fg(yellow)], "Elementos acumulados pelo inimigo:"), nl,
    print_elementos_acumulados(ElementosInimigo), nl,
    nl.

print_elementos_acumulados([]) :-
    centraliza("Nenhum elemento acumulado.").
print_elementos_acumulados([[Fogo | _], [Agua | _], [Neve | _]]) :-
    exibir_quantidade_com_cor(fogo, Fogo),
    exibir_quantidade_com_cor(agua, Agua),
    exibir_quantidade_com_cor(neve, Neve).


print_poderes_ativos(PoderesJogador, PoderesInimigo) :-
    saidas:centraliza_format([bold, fg(cyan)], "Poderes Ativos do Jogador:"), nl,
    print_lista_poderes(PoderesPlayer), nl, nl,
    saidas:centraliza_format([bold, fg(red)], "Poderes Ativos do Inimigo:"), nl,
    print_lista_poderes(PoderesInimigo),nl.


print_lista_poderes([]) :-
    saidas:centraliza("Nenhum poder ativo."), nl.
print_lista_poderes([Poder | Resto]) :-
    carta:mostrar_poder(Poder, NomePoder),
    saidas:centraliza(NomePoder),
    print_lista_poderes(Resto).

exibir_quantidade_com_cor(_, 0) :- !.
exibir_quantidade_com_cor(Elemento, Quantidade) :-
    mostrar_elemento(Elemento, NomeElemento, Cor),
    string_concat(NomeElemento, ": ", Prefixo),
    number_string(Quantidade, QuantidadeString),
    string_concat(Prefixo, QuantidadeString, TextoCompleto),
    centraliza_format(Cor, TextoCompleto).

exibir_placar(PlacarJogador, PlacarInimigo) :-
    format(string(PlacarTexto), "Placar: ~d x ~d", [PlacarJogador, PlacarInimigo]),
    saidas:centraliza(PlacarTexto),
    nl.