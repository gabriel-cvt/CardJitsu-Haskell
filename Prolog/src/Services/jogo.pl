:- module(jogo, [iniciar_jogo/1]).

:- use_module('../Types/carta.pl').
:- use_module('../Types/baralho.pl').
:- use_module('../Types/deque.pl').
:- use_module('../Services/batalha.pl').
:- use_module('./Services/saidas.pl').
:- use_module('./src/Util/lib.pl').
:- dynamic estado_jogo/8.

EstadoJogo(baralhoJogador, elementosJogador, baralhoInimigo, elementosInimigo, dequeJogador, dequeInimigo, pilhaPoderJogador, pilhaPoderInimigo).
estado_jogo([], ([], [], []), [], ([], [], []), [], [], [], []).

iniciar_jogo(Vencedor) :- 
    baralho:novo_baralho(BaralhoJogador),
    baralho:novo_baralho(BaralhoInimigo),
    deque:novo_deque(BaralhoJogador, DequeJogador, RestoJogador),
    deque:novo_deque(BaralhoInimigo, DequeInimigo, RestoInimigo),
    assert(estado_jogo(RestoJogador, ([], [], []), RestoInimigo, ([], [], []), DequeJogador, DequeInimigo, [], [])),
    loop_rodada(Vencedor).

loop_rodada(Vencedor) :- 
    estado_jogo(BaralhoJogador, ElementosJogador, BaralhoInimigo, ElementosInimigo, DequeJogador, DequeInimigo, PilhaPoderJogador, PilhaPoderInimigo),
    loop_rodada_print,
    determinar_poder_proibido(PilhaPoderInimigo),
    deque:mostrar_deque(DequeJogador),
    saidas:centraliza_format([fg(yellow)], "Escolha uma carta para jogar [1-5]:"),
    
    lib:input_to_number(Escolha),
    EscolhaAjustada is Escolha - 1,
    
    deque:jogar_carta(EscolhaAjustada, DequeJogador, CartaJogador, NovoDequeJogador),
    atualiza_deque_jogador(NovoDequeJogador),
    verificar_vencedor(ElementosJogador, ElementosInimigo),
    
    (verifica_fim_jogo(ElementosJogador, ElementosInimigo, Vencedor) -> ! ; loop_rodada(Vencedor)).

% Atualiza o DequeJogador no estado do jogo
atualiza_deque_jogador(NovoDequeJogador) :-
    % Remove o estado atual
    retract(estado_jogo(BaralhoJogador, ElementosJogador, BaralhoInimigo, ElementosInimigo, _, DequeInimigo, PilhaPoderJogador, PilhaPoderInimigo)),
    
    % Adiciona o estado atualizado com o novo DequeJogador
    assert(estado_jogo(BaralhoJogador, ElementosJogador, BaralhoInimigo, ElementosInimigo, NovoDequeJogador, DequeInimigo, PilhaPoderJogador, PilhaPoderInimigo)).


determinar_poder_proibido([], none).  % Se não há poderes ativos, nenhum elemento está bloqueado
determinar_poder_proibido([bloquear(Elemento) | _], Elemento).  % Se o primeiro poder na pilha é de bloqueio, o elemento é proibido
determinar_poder_proibido([_ | RestoPoderes], ElementoProibido) :-
determinar_poder_proibido(RestoPoderes, ElementoProibido).  % Caso contrário, continua verificando a pilha


verificar_vencedor(ElementosJogador, ElementosInimigo):-
    (verifica_vencedor(ElementosJogador)
    -> saidas:centraliza_format([fg(green)], "Você venceu o jogo!")
    ; (verifica_vencedor(ElementosInimigo)
    -> saidas:centraliza_format([fg(green)], "O inimigo venceu o jogo!")
    ; true)).


loop_rodada_print:-
    lib:clearScreen,

    saidas:centraliza("Seus elementos acumulados :"),
    mostrar_elementos(jogador),

    saidas:centraliza("Elementos Acumulados pelo inimigo :"),
    mostrar_elementos(inimigo),

    saidas:centraliza("Poder Ativo:"),
    mostrar_poderes.

mostrar_elementos(jogador):-
    estado_jogo(_, ElementosJogador, _, _, _, _, _, _),
    obter_tamanho_elementos(ElementosJogador, TamanhoFogo, TamanhoAgua, TamanhoNeve),
    centraliza_com_barra("FOGO", TamanhoFogo),
    centraliza_com_barra("AGUA", TamanhoAgua),
    centraliza_com_barra("Neve", TamanhoNeve).

mostrar_elementos(inimigo):-
    estado_jogo(_, _, _, ElementosInimigo, _, _, _, _),
    obter_tamanho_elementos(ElementosInimigo, TamanhoFogo, TamanhoAgua, TamanhoNeve),
    centraliza_com_barra("FOGO", TamanhoFogo),
    centraliza_com_barra("AGUA", TamanhoAgua),
    centraliza_com_barra("Neve", TamanhoNeve).

obter_tamanho_elementos([[Fogo], [Agua], [Neve]], TamanhoFogo, TamanhoAgua, TamanhoNeve) :-
    length(Fogo, TamanhoFogo),
    length(Agua, TamanhoAgua),
    length(Neve, TamanhoNeve).


poderes_ativos:- 
    write("teste").
