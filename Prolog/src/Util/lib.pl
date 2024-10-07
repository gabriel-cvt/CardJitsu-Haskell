:- module(lib, [pressionar_tecla/0, get_user_input/1, clearScreen/0, input_to_number/1, loading/0, verificar_faixa/0]).

:- use_module(library(readutil)).
:- use_module(library(time)).
:- use_module(library(sleep)).
:- use_module('./src/Services/saidas').
:- use_module('./src/Types/player.pl').
:- use_module('./src/Types/faixa.pl').

pressionar_tecla :-
    saidas:centraliza_format([bold, fg(yellow)], 'Pressione a tecla ENTER para continuar...'), nl,
    read_line_to_string(user_input, _), nl,
    true.

get_user_input(Input) :-
    nl, nl,
    saidas:centraliza_format([fg(yellow)], "Digite sua escolha: "),
    read_line_to_string(user_input, Input), nl.

clearScreen :- tty_clear.

input_to_number(N) :-
    get_user_input(InputAux),
    normalize_space(atom(Input), InputAux),
    (atom_number(Input, N) -> true; N = -1).

loading_bar(Progress) :-
    BarSize = 50,
    Hashes is (Progress * BarSize) // 100,
    Dashes is BarSize - Hashes,
    
    length(HashList, Hashes),
    maplist(=('#'), HashList),
    atomic_list_concat(HashList, '', HashPart),
    
    length(DashList, Dashes),
    maplist(=('-'), DashList),
    atomic_list_concat(DashList, '', DashPart),
    
    atomic_list_concat([HashPart, DashPart], BarPart),

    atomic_list_concat(['[', BarPart, '] ', Progress, '%'], Bar),

    saidas:tamanho_terminal(Width),
    string_length(Bar, BarLength),
    Padding is (Width - BarLength) // 2,

    format('\r~*|[', [Padding]),
    ansi_format([fg(green)], '~s', [BarPart]),
    format('] ~d%', [Progress]),

    flush_output,
    
    sleep(0.03).

loading :- 
    nl, forall(between(0, 100, Progress), loading_bar(Progress)), nl.