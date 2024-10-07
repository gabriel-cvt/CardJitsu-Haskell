:- module(controle_jogo, [start_partida/1]).

% nome do bot como um fato dinâmico e global, para não precisar passar como parâmetro em toda regra
:- dynamic nome_bot/1. 

:- use_module('./src/Controller/controleCarregamento.pl').
:- use_module('./src/Services/salvamentos.pl').
:- use_module('./src/Types/faixa.pl').
:- use_module('./src/Services/saidas.pl').
:- use_module('./src/Services/jogo.pl').


start_partida(NomeBot) :-
    assert(nome_bot(NomeBot)),
    rodada(1).

rodada(1) :- 
    jogo:iniciar_jogo(0,0,Vencedor),
    nome_bot(NomeBot),
    lib: clearScreen,
    
    (Vencedor
    ->  string_concat(NomeBot, ': Você ganhou a primeira, mas a próxima irei te destruir.', Mensagem),
        saidas:centraliza(Mensagem), lib:loading,
        rodada(2, ganhou)
    ;   string_concat(NomeBot, ': Você está levando isso a sério? Dessa forma nunca conseguirá ser um Mestre ninja.', Mensagem),
        saidas:centraliza(Mensagem), lib:loading,
        rodada(2, perdeu)).

% Segunda rodada, jogador ganhou a primeira
rodada(2, ganhou) :-
    jogo:iniciar_jogo(1,0,SegundaRodada),

    (SegundaRodada -> resultado(vitoria) ; rodada(3)).

% Segunda rodada, jogador perdeu a primeira
rodada(2, perdeu) :-
    jogo:iniciar_jogo(0,1,SegundaRodada),
    (SegundaRodada -> rodada(3) ; resultado(derrota)).

% Desempate
rodada(3) :- 
    nome_bot(NomeBot),
    lib:clearScreen,

    string_concat('Pelo visto a batalha está difícil com ', NomeBot, Parte1),
    string_concat(Parte1, ', será necessário um desempate!\n', Mensagem),
    saidas:centraliza(Mensagem),
    
    lib:loading,
    
    jogo:iniciar_jogo(1,1,Vencedor),
    (Vencedor -> resultado(vitoria) ; resultado(derrota)).

% Jogador Ganhou
resultado(vitoria) :- 
    salvamentos:up_jogador_partida(Jogador),
    player:get_nome(Jogador, Nome),
    player:get_faixa(Jogador, Faixa),
    nome_bot(NomeBot),
    
    lib:clearScreen,
    string_concat('Parabéns pela vitória ', Nome, MensagemVitoria),
    saidas:centraliza(MensagemVitoria),
    
    string_concat('Após derrotar ', NomeBot, Parte1),
    string_concat(Parte1, ' bravamente, você conquistou a faixa ', Parte2),
    string_concat(Parte2, Faixa, Parte3),
    string_concat(Parte3, '!\n', MensagemFinal),
    saidas:centraliza(MensagemFinal),
    
    lib:pressionar_tecla,
    fim_partida.

% Jogador Perdeu
resultado(derrota) :- 
    nome_bot(NomeBot),
    
    lib:clearScreen,
    string_concat(NomeBot, ': Não foi dessa vez, melhore se quiser me desafiar novamente...\n', Mensagem),
    saidas:centraliza(Mensagem),
    
    lib:pressionar_tecla,
    fim_partida.

% Opções pós partida
fim_partida :-
    lib:clearScreen,
    saidas:centraliza_format([bold, fg(blue)],'Deseja continuar jogando ou voltar ao Dojo?\n'), nl,
    saidas:centraliza_format([fg(blue)],'(1) Continuar Jogando    (2) Voltar ao Dojo\n'),

    lib:input_to_number(Escolha),
    opcao_pos_jogo(Escolha).

opcao_pos_jogo(1) :-
    controle_carregamento:carregar_fase.

opcao_pos_jogo(2) :-
    controle_menu: menu.

opcao_pos_jogo(_) :-
    saidas:centraliza("Opção inválida, tente novamente.\n"),
    lib:pressionar_tecla,
    fim_partida.