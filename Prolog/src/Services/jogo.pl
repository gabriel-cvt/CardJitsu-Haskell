:- module(jogo, [iniciar_jogo/1]).

:- use_module('../Types/carta.pl').
:- use_module('../Types/baralho.pl').
:- use_module('../Types/deque.pl').
:- use_module('../Services/batalha.pl').
:- use_module('./Services/saidas.pl').
:- use_module('./src/Util/lib.pl').
:- use_module('.src/Types/elemento.pl').
:- dynamic estado_jogo/8.

% Estrutura do estado do jogo
estado_jogo(baralhoJogador, elementosJogador, baralhoInimigo, elementosInimigo, dequeJogador, dequeInimigo, pilhaPoderJogador, pilhaPoderInimigo).
estado_jogo([], ([], [], []), [], ([], [], []), [], [], [], []).

% Inicia o jogo
iniciar_jogo(Vencedor) :- 
    baralho:novo_baralho(BaralhoJogador),
    baralho:novo_baralho(BaralhoInimigo),
    deque:novo_deque(BaralhoJogador, DequeJogador, RestoJogador),
    deque:novo_deque(BaralhoInimigo, DequeInimigo, RestoInimigo),
    assert(estado_jogo(RestoJogador, ([], [], []), RestoInimigo, ([], [], []), DequeJogador, DequeInimigo, [], [])),
    loop_jogo(estado_jogo(RestoJogador, ([], [], []), RestoInimigo, ([], [], []), DequeJogador, DequeInimigo, [], []), (0, 0)),
    (verifica_vencedor -> Vencedor = jogador; Vencedor = adversario).


loop_jogo(Estado, (PlacarJogador, PlacarInimigo)) :-
    lib:clearScreen,
    exibir_placar(PlacarJogador, PlacarInimigo),
    exibir_elementos(Estado),
    exibir_poderes(Estado),
    
    determinar_elementos_proibidos(Estado, ElementoProibidoPlayer, ElementoProibidoInimigo),
    
    jogador_faz_jogada(Estado, CartaJogadaPlayer),
    (   CartaJogadaPlayer = none 
    ->  saidas:centraliza_format([bold, fg(red)], "Valor de carta inválido, tente novamente!"), nl, lib:pressionar_tecla,
        loop_jogo(Estado, (PlacarJogador, PlacarInimigo))
    ;   verifica_jogada_valida(CartaJogadaPlayer, ElementoProibidoPlayer)
    ->  processar_jogada(Estado, CartaJogadaPlayer, ElementoProibidoInimigo, PlacarJogador, PlacarInimigo)
    ;   loop_jogo(Estado, (PlacarJogador, PlacarInimigo))).

exibir_placar(PlacarJogador, PlacarInimigo) :-
    format(string(PlacarTexto), "Placar: ~d x ~d", [PlacarJogador, PlacarInimigo]),
    saidas:centraliza(PlacarTexto),
    nl.

exibir_poderes(Estado) :-
    print_poderes_ativos(Estado),
    nl.

determinar_elementos_proibidos(Estado, ElementoProibidoPlayer, ElementoProibidoInimigo) :-
    (pilha_poder_inimigo(Estado) = [bloquear(Elemento) | _] 
    ->  ElementoProibidoPlayer = Elemento
    ;   ElementoProibidoPlayer = none),
    (pilha_poder_player(Estado) = [bloquear(Elemento) | _]
    ->  ElementoProibidoInimigo = Elemento
    ;   ElementoProibidoInimigo = none).

jogador_faz_jogada(Estado, CartaJogada) :-
    Estado = estado_jogo(_, _, _, _, DequeJogador, _, _, _),
    saidas:centraliza_format([bold, fg(yellow)], "Cartas do deque:\n"),
    deque:exibir_deque(DequeJogador),
    lib:input_to_number(Input),
    (deque:jogar_carta(Input, DequeJogador, CartaJogada, NovoDeque)
    ->  NovoEstado = estado_jogo(_, _, _, _, NovoDeque, _, _, _),
        assertz(NovoEstado),
        retract(Estado)
    ;   CartaJogada = none).

verifica_jogada_valida(CartaJogada, ElementoProibido) :-
    (ElementoProibido \= none, carta_elemento(CartaJogada, ElementoProibido) 
    ->  format("Não é permitido jogar a carta do elemento ~w por causa do poder bloqueado pelo jogador.\n", [ElementoProibido]), false
    ;   true).

processar_jogada(Estado, CartaJogada, ElementoProibidoInimigo, PlacarJogador, PlacarInimigo) :-
    Estado = estado_jogo(BaralhoJogador, _, BaralhoInimigo, _, DequeJogador, DequeInimigo, PilhaPoderJogador, PoderesInimigo),
    
    %atualizar_pilha_poder(CartaJogada, PilhaPoderJogador, NovaPilhaPlayer),
    deque:completar_deque(DequeJogador, Baralho,Jogado NovoDequePlayer, BaralhoPlayerRestante),
    inimigo_pegar_carta(BaralhoInimigo, ElementoProibidoInimigo, CartaJogadaInimigo, BaralhoInimigoRestante),
    deque:completar_deque(DequeInimigo, BaralhoInimigo, NovoDequeInimigo, BaralhoInimigoRestanteFinal),
    (CartaJogadaInimigo = none
    ->  format("O inimigo não tem cartas para jogar. O jogo acabou!\n", []), true
    ;   format("O inimigo jogou: ~w\n", [CartaJogadaInimigo]),
        resolver_combate(Estado, CartaJogada, CartaJogadaInimigo, NovoDequePlayer, NovoDequeInimigo, BaralhoPlayerRestante, BaralhoInimigoRestanteFinal, PlacarJogador, PlacarInimigo)),
        
    NovoEstado = estado_jogo(BaralhoJogador, _, BaralhoInimigo, _, NovoDequePlayer, NovoDequeInimigo, NovaPilhaPlayer, PoderesInimigo),
    retract(Estado),
    assertz(NovoEstado).

resolver_combate(Estado, CartaJogadaPlayer, CartaJogadaInimigo, NovoDequePlayer, NovoDequeInimigo, BaralhoPlayerRestante, BaralhoInimigoRestanteFinal, PlacarJogador, PlacarInimigo) :-
    poder_player(PoderPlayer, Estado),
    poder_inimigo(PoderInimigo, Estado),
    
    vencedor(CartaJogadaPlayer, CartaJogadaInimigo, PoderPlayer, PoderInimigo, Vencedor),
    atualizar_elementos(Vencedor, Estado, CartaJogadaPlayer, CartaJogadaInimigo, NovoEstado),
    
    (verifica_vencedor(NovoEstado)
    ->  format("Você venceu o jogo!\n", []), true
    ;   verifica_vencedor_inimigo(NovoEstado)
    ->  format("O inimigo venceu o jogo!\n", []), true
    ;   loop_jogo(NovoEstado, (PlacarJogador, PlacarInimigo))).

exibir_elementos(Estado) :-
    Estado = estado_jogo(_, (ElementosJogador, _, _), _, (ElementosInimigo, _, _), _, _, _, _),
    saidas:centraliza("Seus elementos acumulados:"),
    print_lista_elementos(ElementosJogador), nl, nl,
    saidas:centraliza("Elementos acumulados pelo inimigo:"),
    print_lista_elementos(ElementosInimigo), nl,
    nl.

% Exibe a lista de elementos
print_lista_elementos([]) :-
    saidas:centraliza("Nenhum elemento acumulado.").
print_lista_elementos([Elemento | Resto]) :-
    mostrar_elemento(Elemento, NomeElemento),
    saidas:centraliza(NomeElemento),
    print_lista_elementos(Resto).

% Exibe os poderes ativos
print_poderes_ativos(Estado) :-
    Estado = estado_jogo(_, _, _, _, _, _, PoderesPlayer, PoderesInimigo),
    saidas:centraliza("Poderes Ativos do Jogador:"),
    print_lista_poderes(PoderesPlayer), nl, nl,
    saidas:centraliza("Poderes Ativos do Inimigo:"),
    print_lista_poderes(PoderesInimigo),nl.

% Exibe a lista de poderes
print_lista_poderes([]) :-
    saidas:centraliza_com_barra("Nenhum poder ativo.", "").
print_lista_poderes([Poder | Resto]) :-
    carta:mostrar_poder(Poder, NomePoder),
    saidas:centraliza_com_barra(NomePoder, ""),
    print_lista_poderes(Resto).

%atualiza_pilha_poder(Estado, NovoPoderes, NovoEstado) :-
%    Estado = estado_jogo(BaralhoJogador, ElementosJogador, BaralhoInimigo, ElementosInimigo, DequeJogador, DequeInimigo, _, PoderesInimigo),
%    NovoEstado = estado_jogo(BaralhoJogador, ElementosJogador, BaralhoInimigo, ElementosInimigo, DequeJogador, DequeInimigo, NovoPoderes, PoderesInimigo).