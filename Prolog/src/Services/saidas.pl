:- module(saidas, [printMenu/0, printNovoJogo/0, ler_instrucoes/0, centraliza/1, texto/1, centraliza_com_barra/2]).

:- use_module('./src/Services/salvamentos.pl').
:- use_module('./src/Util/lib.pl').
:- use_module(library(sleep)).
:- use_module(library(ansi_term)).
:- use_module(library(process)).

printMenu :-
    centraliza_format([bold, fg(yellow)], "          ___                   _         __                      __       _        __ \n"),
    centraliza_format([bold, fg(yellow)], "         / _ )___ __ _    _  __(_)__  ___/ /__    ___ ____    ___/ /__    (_)__    / / \n"),
    centraliza_format([bold, fg(yellow)], "        / _  / -_)  ' \\  | |/ / / _  / _  / _ \\  / _ `/ _ \\  / _  / _ \\  / / _ \\  /_/  \n"),
    centraliza_format([bold, fg(yellow)], "       /____/\\__/_/_/_/  |___/_/_//_/\\_,_/\\___/  \\_,_/\\___/  \\_,_/\\___/_/ /\\___/ (_)   \n"),
    centraliza_format([bold, fg(yellow)], "                                                                         |___/                 \n"),
    centraliza_format([bold, fg(blue)], "Qual vai ser a sua escolha para hoje?\n"),
    write("\n"),
    centraliza_format([fg(blue)], "(1) Começar novo jogo    (2) Carregar jogo    (3) Checar faixas    (4) Instruções    (5) Sair do jogo\n").

printNovoJogo :-
    centraliza("Começando uma nova aventura ninja...\n"),
    centraliza("Qual é o seu nome?").

ler_instrucoes :-
    salvamentos:carregar_instrucoes(Instrucoes),
    imprimir_instrucoes(Instrucoes).


imprimir_instrucoes([]) :- nl.
imprimir_instrucoes([Linha|Linhas]) :-
    centraliza(Linha),
    imprimir_instrucoes(Linhas).


tamanho_terminal(Colunas) :-
    process_create(path(stty), ['size'], [stdout(pipe(Out))]),
    read_line_to_string(Out, Result),
    split_string(Result, " ", "", [_LinhasStr, ColunasStr]),
    number_string(Colunas, ColunasStr).

% Centraliza uma linha de texto
centraliza(Text) :-
    tamanho_terminal(Colunas),
    string_length(Text, TextLength),
    Espacos is (Colunas - TextLength) // 2,
    tab(Espacos),
    writeln(Text).

centraliza_format(Estilo, Texto) :-
    tamanho_terminal(Colunas),
    string_length(Texto, TextLength),
    Espacos is (Colunas - TextLength) // 2,
    tab(Espacos),
    ansi_format(Estilo, Texto, []).

% Alinhado
centraliza_com_barra(Titulo, Texto) :-
    tamanho_terminal(Colunas),
    PosBarra is 30,
    string_length(Titulo, TituloLength),
    EspacosTitulo is PosBarra - TituloLength - 2,  % Subtraindo para espaço antes do "|"
    tab(EspacosTitulo),
    write('| '), write(Titulo),
    string_length(Texto, TextLength),
    EspacosTexto is Colunas - PosBarra - TextLength - TituloLength - 2,
    tab(EspacosTexto),
    writeln(Texto).

centraliza_com_cor(TextoAntes, PalavraColorida, TextoDepois, Estilo) :-
    atomic_list_concat([TextoAntes, PalavraColorida, TextoDepois], " ", TextoCompleto),
    tamanho_terminal(Colunas),
    string_length(TextoCompleto, TextLength),
    Espacos is (Colunas - TextLength) // 2,
    tab(Espacos),
    write(TextoAntes),  
    ansi_format(Estilo, PalavraColorida, []),  
    write(TextoDepois), 
    nl.  

texto(branca) :- 
    centraliza_format([bold, fg(white)], "O ruivo é a primeira pessoa que aparece no Dojo"), nl,
    sleep(0.75),
    centraliza_com_cor("", "Ruivo", ": você é novo por aqui, não é?", [bold, fg(red)]),
    sleep(0.75),
    centraliza_com_cor("", "Ruivo", ": vamos a um duelo para você entender como é o CardJitsu\n", [bold, fg(red)]),
    sleep(0.75).


texto(azul) :-
    centraliza_format([bold, fg(white)], "A bruxa da Neve se aproxima de forma sorrateira após você ganhar do Ruivo"), nl,
    sleep(0.75),
    centraliza_com_cor("", "Bruxa da Neve", ": O ruivo é mesmo um bobalhão, mas de minhas garras gélidas você não escapará!", [bold, fg(cyan)]),
    sleep(0.75),
    centraliza("Não restam opções para subir de faixa no dojo a não ser derrotando ela!\n"),
    sleep(0.75).

texto(roxa) :-
    centraliza_com_cor("", "Cavaleiro do Mar", ": para derrotar a bruxa... requer um poder imenso", [bold, fg(blue)]),
    sleep(0.75),
    centraliza("Como você conseguiu fazer isso, jovem ninja?"),
    sleep(0.75),
    centraliza("Você respondeu que somente batalhando para saber disso"),
    sleep(0.75),
    centraliza_com_cor("", "Cavaleiro do Mar", ": Então é assim ?\n", [bold, fg(blue)]),
    centraliza_format([bold, fg(white)], "É HORA DO DUELO!\n"),
    sleep(0.75).

texto(marrom) :-
    centraliza("Após a derrota do Cavaleiro do Mar, o seu melhor amigo Punhos de Fogo apareceu correndo!"),
    sleep(0.75),
    centraliza("Ele está devastado! Qualquer coisa ao redor dele está em chamas!!"),
    sleep(0.75),
    centraliza_com_cor("", "Punhos de Fogo", ": Não me importa quem é você ou o que você quer, minhas chamas irão te exterminar", [bold, fg(red)]),
    sleep(0.75),
    centraliza("Jovem ninja, como você ganhará dele??\n"),
    sleep(0.75).

texto(preta):- 
    centraliza("Olhos de Falcão, após ver todos os seus aprendizes derrotados, chegou em você para conversar"),
    sleep(0.75),
    centraliza_com_cor("", "Olhos de Falcão", ": escute o que tenho a dizer...", [bold, fg(yellow)]),
    sleep(0.75),
    centraliza("Todas as pessoas verdadeiramente fortes são gentis."),
    sleep(0.75),
    centraliza("Não pense que ganhar de todos irá lhe tornar mais forte do que você é agora.."),
    sleep(0.75),
    centraliza("Chega de palavras agora, chegou a hora..."),
    sleep(0.75),
    centraliza_format([bold, fg(white)], "É HORA DO DUELO!\n"),
    sleep(0.75).

texto(final) :- 
    centraliza("Após derrotar o grande Sensei, você conseguiu alcançar algo que todos desejam..."),
    sleep(0.75),
    centraliza("Você encontrou finalmente o One pi..."),
    sleep(0.75),
    centraliza_format([bold, fg(yellow)], "A GRANDE MÁSCARA NINJA!\n"),
    sleep(0.75),
    centraliza("Após você conquistar esse feito, só restam duas opções...\n"),
    sleep(0.75),
    centraliza_format([bold, fg(blue)], "(1) Abandonar tudo que você conseguiu até agora e começar uma nova Jornada"), nl,
    centraliza_format([bold, fg(blue)], "(2) Finalizar o jogo como um grande ninja"),
    sleep(0.75).