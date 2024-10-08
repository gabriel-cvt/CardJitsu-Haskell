:- module(jogo, [iniciar_jogo/3,
                 loop_jogo/1,
                 update_elementos/3,
                 mostrar_elementos/1,
                 checa_vencedor/1]).

:- use_module('../Util/lib.pl').
:- use_module('../Types/elemento.pl').
:- use_module('../Types/carta.pl').
:- use_module('../Types/baralho.pl').
:- use_module('../Types/deque.pl').
:- use_module('batalha.pl').
:- use_module('saidas.pl').

:- dynamic estado_jogo/8.

estado_jogo(0, 0, [], [], [], [], [], []).

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
                        [[], [], []])),

    loop_jogo(Vencedor).

loop_jogo(Vencedor):-
    retract(estado_jogo(Jogador, Inimigo,
                        BaralhoJogador, BaralhoInimigo,
                        DequeJogador, DequeInimigo,
                        ElementosJogador, ElementosInimigo)),
    
    print_estado_jogo,

    lib:input_to_number(Index),

    (Index < 1; Index > 5 ->
        writeln("Entrada inválida. Tente novamente."),
        loop_jogo(Vencedor) 
    ;
        deque:jogar_carta(Index, DequeJogador, CartaJogador, DequeJogador2),
        baralho:pegar_carta(BaralhoInimigo, CartaInimigo, NewBaralhoInimigo),
        
        deque:completar_deque(DequeJogador2, BaralhoJogador, NewDequeJogador, NewBaralhoJogador),

        mostrar_deque(NewDequeJogador, StringDeque),
        writeln(StringDeque),

        CartaJogador = carta(EleJogador,_),
        CartaInimigo = carta(EleInimigo,_),
        batalha:combate(CartaJogador, CartaInimigo, Ganhador),

        writeln("Ganhador do combate: "), writeln(Ganhador),

        (Ganhador = 1 ->
            update_elementos(EleJogador, ElementosJogador, NewElementosJogador),
            (checa_vencedor(NewElementosJogador) ->
                Vencedor is 1, !
            ;
                assertz(estado_jogo(Jogador, Inimigo,
                                    NewBaralhoJogador, NewBaralhoInimigo,
                                    NewDequeJogador, DequeInimigo,
                                    NewElementosJogador, ElementosInimigo)),
                loop_jogo(Vencedor)
            )
        ; Ganhador = 2 ->
            update_elementos(EleInimigo, ElementosInimigo, NewElementosInimigo),
            (checa_vencedor(NewElementosInimigo) ->
                Vencedor is 2, !
            ;
                assertz(estado_jogo(Jogador, Inimigo,
                                    NewBaralhoJogador, NewBaralhoInimigo,
                                    NewDequeJogador, DequeInimigo,
                                    ElementosJogador, NewElementosInimigo)),
                loop_jogo(Vencedor)
            )
        )
    ).

update_elementos(Elemento, Elementos, NovoElementos):-
    (Elemento = fogo -> NovoElementos = [[fogo|Elementos], _, _]
    ; Elemento = agua -> NovoElementos = [_, [agua|Elementos], _]
    ; Elemento = neve -> NovoElementos = [_, _, [neve|Elementos]]).

mostrar_elementos([Fogo, Agua, Neve]):-
    length(Fogo, QtdFogo),
    length(Agua, QtdAgua),
    length(Neve, QtdNeve),

    write("| FOGO : "), write(QtdFogo), nl,
    write("| ÁGUA : "), write(QtdAgua), nl,
    write("| NEVE : "), write(QtdNeve), nl.

checa_vencedor([Fogo, Agua, Neve]):-
    length(Fogo, QtdFogo),
    length(Agua, QtdAgua),
    length(Neve, QtdNeve),

    (QtdFogo > 0, QtdAgua > 0, QtdNeve > 0
    ;
    QtdFogo = 3
    ;
    QtdAgua = 3
    ;
    QtdNeve = 3).

print_estado_jogo :-
    retract(estado_jogo(Jogador, Inimigo,
                        BaralhoJogador, BaralhoInimigo,
                        DequeJogador, DequeInimigo,
                        ElementosJogador, ElementosInimigo)),
    lib:clearScreen,
    exibir_placar(Jogador, Inimigo),
    exibir_elementos(ElementoJogador, ElementoInimigo), nl,

    saidas:centraliza_format([bold, fg(red)], "Cartas do deque:\n"),
    deque:exibir_deque(DequeJogador), nl, nl,

    saidas:centraliza_format([bold, fg(blue)], "Escolha uma carta para jogar [1-5]:").

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
