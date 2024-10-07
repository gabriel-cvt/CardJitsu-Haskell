:- module(controle_jogo, [start_partida/1]).

% nome do bot como um fato dinâmico e global, para não precisar passar como parâmetro em toda regra
:- dynamic nome_bot/1. 

:- use_module('./src/Services/jogo.pl').
:- use_module('./src/Controller/controleFases.pl').
:- use_module('./src/Services/salvamentos.pl').
:- use_module('./src/Types/faixa.pl').


start_partida(NomeBot) :-
    assertz(nome_bot(NomeBot)),
    rodada(1).

rodada(1) :- 
    jogo:iniciar_jogo([0, 0], Vencedor),
    nome_bot(NomeBot),

    lib:loading,
    (Vencedor
    ->  format('~s: Você ganhou a primeira, mas a próxima irei te destruir.~n', [NomeBot]),
        rodada(2, ganhou)
    ;   format('~s: Você está levando isso a sério? Dessa forma nunca conseguirá ser um Mestre ninja.~n', [NomeBot]),
        rodada(2, perdeu)).

rodada(2, ganhou) :-
    jogo:iniciar_jogo([1, 0], SegundaRodada),

    (SegundaRodada -> resultado(vitoria) ; rodada(3)).

rodada(2, perdeu) :-
    jogo:iniciar_jogo([0, 1], SegundaRodada),
    (SegundaRodada -> rodada(3) ; resultado(derrota)).

rodada(3) :-
    nome_bot(NomeBot),
    format('Pelo visto a batalha está  difícil com ~s, será necessário um desempate!~n', [NomeBot]),

    lib:loading,

    jogo:iniciar_jogo([1, 1], Vencedor),
    (Vencedor -> resultado(vitoria) ; resultado(derrota)).

% Jogador Ganhou
resultado(vitoria) :-
    salvamentos:up_jogador_partida(Jogador),
    player:get_nome(Jogador, Nome),
    player:get_faixa(Jogador, Faixa),
    nome_bot(NomeBot),

    format('Parabéns pela vitória ~s!', [Nome]),
    format('Após derrotar ~s bravamente, você conquistou a faixa ~s!~n', [NomeBot], [Faixa]),

    lib:pressionar_tecla,
    interface_pos_jogo.

% Jogador Perdeu
resultado(derrota) :-

    nome_bot(NomeBot),
    format('~s: Não foi dessa vez, melhore se quiser me desafiar novamente...~n', [NomeBot]),

    lib:pressionar_tecla,
    interface_pos_jogo.

interface_pos_jogo :-

    format('Deseja continuar jogando ou voltar ao Dojo?~n'),
    format('1 - Continuar Jogando~n'),
    format('2 - Voltar ao Dojo~n'),

    lib:input_to_number(Escolha),
    opcao_pos_jogo(Escolha).

opcao_pos_jogo(1) :-
    controle_fase:carregar_fase.

opcao_pos_jogo(2) :-
    controle_menu: menu.

opcao_pos_jogo(_) :-
    write("Opção inválida, tente novamente.\n"),
    lib:pressionar_tecla,
    interface_pos_jogo.