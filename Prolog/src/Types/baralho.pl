:- module(baralho, [novo_baralho/1,
                    pegar_carta/3,
                    extrair_cartas/4,
                    embaralhar/3]).
                    
:- use_module('./src/Types/carta.pl').

baralho([Carta]) :- carta(Carta).

% Regra que gera todas as possíveis cartas de um elemento
% Recebe um Elemento e cria todas as cartas necessárias.
gerar_cartas_elemento(Elemento, Cartas) :-
    gerar_cartas_elemento(Elemento, 1, Cartas).

gerar_cartas_elemento(_, Valor, []) :- Valor < 1, Valor > 12.
gerar_cartas_elemento(Elemento, Valor, [Carta | Cartas]) :-
    Valor >= 1, Valor =< 12,
    (   Valor =< 8 -> Poder = null
    ;   Valor = 9 -> Poder = mais_dois
    ;   Valor = 10 -> Poder = menos_dois
    ;   Valor = 11 -> Poder = bloquear(Elemento)
    ;   Valor = 12 -> Poder = inverte
    ),
    carta(Elemento, Valor, Poder),
    ProxValor is Valor + 1,
    gerar_cartas_elemento(Elemento, ProxValor, Cartas).

% Regra responsável por criar um novo baralho,
% gera cartas de todos os elementos, concatena
% e as retorna embaralhadas.
novo_baralho(Baralho) :-
    gerar_cartas_elemento(fogo, CartasFogo),
    gerar_cartas_elemento(agua, CartasAgua),
    gerar_cartas_elemento(neve, CartasNeve),
    append(CartasFogo, CartasAgua, FogoAgua),
    append(FogoAgua, CartasNeve, BaralhoTemp),
    embaralhar(BaralhoTemp, Baralho).

% Regra que pega uma carta do baralho
% retorna a primeira carta e o baralho atualizado.
pegar_carta([], none, []).
pegar_carta([Carta|Cartas], Carta, Cartas).

% Regra que extrai n cartas do baralho
% retorna uma lista de cartas que são acumuladas
% na recursão e também retorna o resto do baralho.
extrair_cartas(0, Baralho, [], Baralho).
extrair_cartas(Qtd, Baralho, [Carta|Cartas], Resto):-
    Qtd > 0,
    pegar_carta(Baralho, Carta, RestoBaralho),
    NextQtd is Qtd - 1,
    extrair_cartas(NextQtd, RestoBaralho, Cartas, Resto).

% Regra que embaralha uma lista.
embaralhar(Lista, ListaEmbaralhada) :-
    length(Lista, N),
    gerar_indices(N, Indices),
    permutar_lista(Lista, Indices, ListaEmbaralhada).

gerar_indices(N, Indices) :-
    findall(X, between(0, N-1, X), Indices).

permutar_lista([], _, []).
permutar_lista(Lista, [Index | Indices], [Elemento | ListaEmbaralhada]) :-
    nth0(Index, Lista, Elemento),
    delete(Lista, Elemento, ListaSemElemento),
    permutar_lista(ListaSemElemento, Indices, ListaEmbaralhada).
