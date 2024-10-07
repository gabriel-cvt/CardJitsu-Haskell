:- module(controle_menu, [menu/0]).

:- use_module('./src/Util/lib.pl').
:- use_module('./src/Services/saidas.pl').
:- use_module('./src/Controller/controleJogo.pl').
:- use_module('./src/Controller/controleCarregamento.pl').
:- use_module('./src/Types/faixa.pl').
:- use_module(library(sleep)).

menu :- 
    lib:clearScreen,
    saidas:printMenu,
    lib:input_to_number(Escolha),
    opcaoMenu(Escolha).

opcaoMenu(1) :-
    controle_carregamento:novo_jogo.

opcaoMenu(2) :-
    controle_carregamento:carregar_jogo.

opcaoMenu(3) :-
    faixa:verificar_faixa,
    lib:pressionar_tecla,
    menu.

opcaoMenu(4) :-
    saidas:ler_instrucoes,
    lib:pressionar_tecla,
    menu.

opcaoMenu(5) :-
    saidas:centraliza("Até a próxima, jovem ninja.\n"),
    saidas:centraliza("Fechando o Dojo..."),
    sleep(1.5),
    halt(0).

opcaoMenu(_) :-
    saidas:centraliza("Opção inválida, tente novamente.\n"),
    lib:pressionar_tecla,
    menu.