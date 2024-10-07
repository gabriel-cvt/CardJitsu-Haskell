:- use_module('./src/Controller/controleMenu.pl').
:- use_module('./src/Util/lib.pl').

main :- start.

start :-
    lib:clearScreen,
    lib:pressionar_tecla,
    controle_menu:menu,
    halt.