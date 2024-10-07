:- module(jogo, [iniciar_jogo/2]).

:- use_module('../Types/carta.pl').
:- use_module('../Types/baralho.pl').
:- use_module('../Types/deque.pl').
:- use_module('../Services/batalha.pl').
:- use_module('./Services/saidas.pl').
:- use_module('./src/Util/lib.pl').
:- dynamic estado_jogo/8.
:- dynamic placar/1.

% Apenas testando o Controler Jogo

% Pro retorno ser booleano mesmo, precisa colocar "!."
% Se não, retorna com endereço de memória junto

EstadoJogo(baralhoJogador, elementosJogador, baralhoInimigo, elementosInimigo, dequeJogador, dequeInimigo, pilhaPoderJogador, pilhaPoderInimigo).
estado_jogo([], ([], [], []), [], ([], [], []), [], [], [], []).

Placar(placar),
placar([0, 0]),

iniciar_jogo(Placar, Vencedor) :- 
    assert(placar(Placar)),

    baralho:novo_baralho(BaralhoJogador),
    baralho:novo_baralho(BaralhoInimigo),

    deque:novo_deque(BaralhoJogador, DequeJogador, RestoJogador),
    deque:novo_deque(BaralhoInimigo, DequeInimigo, RestoInimigo),

    assert(estado_inicial(RestoJogador, ([], [], []), RestoInimigo, ([], [], []), DequeJogador, DequeInimigo, [], [])),
    loop_jogo(Placar).


loop_jogo(Placar) :- 
    loop_jogo_print,

    elemento_proibido_jogador(pilhaPoderInimigo),
    elemento_proibido_inimigo(pilhaPoderJogador),


    saidas:centraliza_format([fg(yellow)], "Escolha uma carta para jogar [1-5]:"),
    deque:mostrar_deque(DequeJogador),

    lib:input_to_number(Escolha),
    EscolhaAjustada is Escolha -1,

    deque:jogar_carta(Escolha, CartaJogador, NovoDequeJogador),

    atualiza_deque_jogador(NovoDequeJogador),
.

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


loop_jogo_print:-
    lib:clearScreen,

    saidas:centraliza("Placar :"),
    saidas:centraliza(Placar),

    saidas:centraliza("Seus elementos acumulados :"),
    mostrar_elementos(),

    saidas:centraliza("Elementos Acumulados pelo inimigo :"),
    mostrar_elementos_inimigo(),

    saidas:centraliza("Poder Ativo:"),
    mostrar_poderes_ativos().

elementos_jogador_saida:-
    estado_jogo(_, ElementosJogador, _, _, _, _, _, _),
    obter_tamanho_elementos(ElementosJogador, TamanhoFogo, TamanhoAgua, TamanhoNeve),
    centraliza_com_barra("FOGO", TamanhoFogo),
    centraliza_com_barra("AGUA", TamanhoAgua),
    centraliza_com_barra("Neve", TamanhoNeve).

elemenetos_inimigo_saida:-
    estado_jogo(_, _, _, ElementosInimigo, _, _, _, _),
    obter_tamanho_elementos(ElementosInimigo, TamanhoFogo, TamanhoAgua, TamanhoNeve),
    centraliza_com_barra("FOGO", TamanhoFogo),
    centraliza_com_barra("AGUA", TamanhoAgua),
    centraliza_com_barra("Neve", TamanhoNeve).

obter_tamanho_elementos([[Fogo], [Agua], [Neve]], TamanhoFogo, TamanhoAgua, TamanhoNeve) :-
    length(Fogo, TamanhoFogo),
    length(Agua, TamanhoAgua),
    length(Neve, TamanhoNeve).