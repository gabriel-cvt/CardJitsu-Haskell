:- module(jogo, [iniciar_jogo/2]).

:- use_module('../Types/carta.pl').
:- use_module('../Types/baralho.pl').
:- use_module('../Types/deque.pl').
:- use_module('../Services/batalha.pl').

% Apenas testando o Controler Jogo

% Pro retorno ser booleano mesmo, precisa colocar "!."
% Se não, retorna com endereço de memória junto

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
