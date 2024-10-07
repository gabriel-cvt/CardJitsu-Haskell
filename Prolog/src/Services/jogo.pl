:- module(jogo, [iniciar_jogo/1]).

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
    retract(estado_jogo(Jogador, Inimigo,
                        BaralhoJogador, BaralhoInimigo,
                        DequeJogador, DequeInimigo,
                        ElementosJogador, ElementosInimigo,
                        PilhaJogador, PilhaInimigo)),

    writeln("Placar: ")
    write(Jogador), write(" X "), write(Inimigo), nl,

    writeln("Escolha uma carta para jogar [1-5]:"),
    input_to_number(Index),
    Index > 0, Index < 6,
    
    deque:jogar_carta(Index, DequeJogador, CartaJogador, DequeJogador2),
    inimigo:inimigo_jogar_carta(BaralhoInimigo, CartaInimigo, BaralhoInimigo2),

    CartaJogador = carta(ElementoJogador, _, PoderJogador),
    CartaInimigo = carta(ElementoInimigo, _, PoderInimigo),

    combate(CartaJogador, CartaInimigo, PJ, PI, Ganhador),
    (Ganhador = 1 ->
        (ElementoJogador = fogo ->
            NovoElementosJogador = [[fogo|ElementosJogador], _, _]
        ; ElementoJogador = agua ->
            NovoElementosJogador = [_, [agua|ElementosJogador], _]
        ; ElementoJogador = neve ->
            NovoElementosJogador = [_, _, [neve|ElementosJogador]]
        ),

        (checa_vencedor(NovoElementosJogador) -> Vencedor is 1, !
            writeln("Fim do jogo, jogador venceu!"), !
        ; 
        assertz(estado_jogo(Jogador, Inimigo, BaralhoJogador, BaralhoInimigo, DequeJogador2, DequeInimigo, NovoElementosJogador, ElementosInimigo, [PJ|PilhaJogador], PilhaInimigo)),
        )


    ; Ganhador = 2 ->
        (ElementoInimigo = fogo ->
            NovoElementosInimigo = [[fogo|ElementosInimigo], _, _]
        ; ElementoInimigo = agua ->
            NovoElementosInimigo = [_, [agua|ElementosInimigo], _]
        ; ElementoInimigo = neve ->
            NovoElementosInimigo = [_, _, [neve|ElementosInimigo]]
        ),

        (checa_vencedor(NovoElementosInimigo) -> Vencedor is 2, !
        ; 
        assertz(estado_jogo(Jogador, Inimigo, BaralhoJogador, BaralhoInimigo, DequeJogador2, DequeInimigo, ElementosJogador, NovoElementosInimigo, PilhaJogador, [PI|PilhaInimigo])),
        )


    ), loop_jogo(Vencedor).


checa_vencedor([Fogo, Agua, Neve]) :-
    (length(Fogo, 1), length(Agua, 1), length(Neve, 1))
    ;
    length(Fogo, 3)
    ;
    length(Agua, 3)
    ;
    length(Neve, 3).
