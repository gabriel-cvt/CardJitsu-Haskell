:- module(jogo, [iniciar_jogo/2]).

:- use_module('../Types/carta.pl').
:- use_module('../Types/baralho.pl').
:- use_module('../Types/deque.pl').
:- use_module('../Services/batalha.pl').
<<<<<<< HEAD
=======
:- use_module('./Services/saidas.pl').
:- use_module('./src/Util/lib.pl').
:- dynamic estado_jogo/8.
:- dynamic placar/1.
>>>>>>> gabriel

% Apenas testando o Controler Jogo

% Pro retorno ser booleano mesmo, precisa colocar "!."
% Se não, retorna com endereço de memória junto

<<<<<<< HEAD
jogar :-
    iniciar_jogo,
    recursao,
    (jogar_turno -> true; true).

iniciar_jogo:-
    novo_baralho(BaralhoJogador),
    novo_baralho(BaralhoInimigo),

    novo_deque(BaralhoJogador, JogadorDeque, BaralhoJogador),
    novo_deque(BaralhoInimigo, JogadorDeque, BaralhoInimigo),

    loop_jogo(0, 0)

loop_jogo(JogadorScore, InimigoScore) :-
    writeln("Seus elementos acumulados:"),
    mostrar_elementos(),

    writeln("Elementos acumulados pelo inimigo:"),
    mostrar_elementos_inimigo(),

    writeln("Poder Ativo:"),
    mostrar_poderes_ativos(),

    % Determinar elemento proibido
    (poder_inimigo(PoderInimigo),
     (PoderInimigo = bloqueio(ElementoProibido) -> true; ElementoProibido = none)
    ; ElementoProibido = none),

    % Jogador faz sua jogada
    writeln("Escolha uma carta para jogar [1-5]:"),
    mostrar_deque_jogador(DequeJogador),
    read(Escolha),
    (Escolha < 1; Escolha > 5 ->
        writeln("Escolha inválida. Tente novamente."),
        pause,
        loop_jogo(JogadorScore, InimigoScore)
    ;
        EscolhaAjustada is Escolha - 1,
        jogar_carta(EscolhaAjustada, DequeJogador, CartaJogadaPlayer, NovoDequeJogador)
    ),
    
    (var(CartaJogadaPlayer) ->
        writeln("Escolha inválida. Tente novamente."),
        pause,
        loop_jogo(JogadorScore, InimigoScore);
    (poder_inimigo(PoderInimigo),
     (PoderInimigo = bloqueio(ElementoProibido),
      elemento_da_carta(CartaJogadaPlayer, ElementoCarta),
      ElementoCarta = ElementoProibido ->
          writeln("Não é permitido jogar a carta do elemento ~w por causa do poder bloqueado pelo inimigo.", [ElementoProibido]),
          pause,
          loop_jogo(JogadorScore, InimigoScore);
      true)),
    
    % Atualiza a pilha de poderes do jogador
    atualizar_pilha_poder(CartaJogadaPlayer, NovaPilhaPlayer),
    
    % Inimigo faz sua jogada
    inimigo_pegar_carta(BaralhoInimigo, ElementoProibido, CartaJogadaInimigo),
    
    (var(CartaJogadaInimigo) ->
        writeln("O inimigo não tem cartas para jogar. O jogo acabou!"),
        pause,
        true;
    writeln("O inimigo jogou:"),
    writeln(CartaJogadaInimigo),
    pause,
    
    % Resolver combate
    poder_jogador(PoderJogador),
    poder_inimigo(PoderInimigo),
    
    (PoderJogador = 0, PoderInimigo = 0 ->
        Vencedor = combate(CartaJogadaPlayer, CartaJogadaInimigo);
    (PoderJogador = 1; PoderInimigo = 1 ->
        Vencedor = combate_invertido(CartaJogadaPlayer, CartaJogadaInimigo);
    Vencedor = combate_com_poder(CartaJogadaPlayer, CartaJogadaInimigo, PoderJogador, PoderInimigo))),
    
    (Vencedor = jogador ->
        NovoElementoJogador = atualizar_elementos(ElementosJogador, CartaJogadaPlayer);
    (Vencedor = inimigo ->
        NovoElementoInimigo = atualizar_elementos(ElementosInimigo, CartaJogadaInimigo);
    true)),
    
    verificar_vencedor(NovoElementoJogador, NovoElementoInimigo),
    loop_jogo(NovoScoreJogador, NovoScoreInimigo)
    )).

% Função para exibir elementos acumulados do jogador
mostrar_elementos() :-
    % Aqui você deve implementar a lógica para exibir os elementos do jogador
    true.

% Função para exibir elementos acumulados do inimigo
mostrar_elementos_inimigo() :-
    % Aqui você deve implementar a lógica para exibir os elementos do inimigo
    true.

% Função para exibir poderes ativos
mostrar_poderes_ativos() :-
    % Aqui você deve implementar a lógica para exibir os poderes ativos
    true.

% Função para mostrar o deque do jogador
mostrar_deque_jogador(DequeJogador) :-
    % Aqui você deve implementar a lógica para exibir o deque do jogador
    true.

% Função para pausar a tela e esperar pelo input do usuário
pause :-
    writeln("Pressione Enter para continuar..."),
    read_line(_).

% Função para verificar o vencedor
verificar_vencedor(ElementosJogador, ElementosInimigo) :-
    (verifica_vencedor(ElementosJogador) ->
        writeln("Você venceu o jogo!");
    (verifica_vencedor(ElementosInimigo) ->
        writeln("O inimigo venceu o jogo!");
    true)).
=======
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
>>>>>>> gabriel
