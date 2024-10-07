:- module(services_batalha, [combate/5]).

:- use_module('./src/Types/carta.pl').
:- use_module('./src/Types/elemento.pl').

% Definindo os possíveis vencedores da rodada
vencedor_rodada(1).
vencedor_rodada(2).
vencedor_rodada(0).

% Combate
combate(Carta1, Carta2, Poder1, Poder2, Vencedor) :-
    combate_por_elemento(Carta1, Carta2, VencedorElementos),
    ( VencedorElementos == jogador ->
        Vencedor = 1
    ; VencedorElementos == adversario ->
        Vencedor = 2
    ; VencedorElementos == nenhum ->
        combate_por_valor(Carta1, Carta2, Poder1, Poder2, Vencedor)
    ).

% Combate baseado no elemento da carta
combate_por_elemento(carta(ElJogador, _, _), carta(ElBot, _, _), Vencedor) :-
    (prioridade_elemento(ElJogador, ElBot) ->
        Vencedor = jogador
    ; prioridade_elemento(ElBot, ElJogador) ->
        Vencedor = adversario
    ; Vencedor = nenhum).

% Combate baseado no valor da carta, considerando poderes
combate_por_valor(carta(_, ValorJogador, _), carta(_, ValorBot, _), Poder1, Poder2, Vencedor) :-
    aplicar_poder(ValorJogador, Poder1, Poder2, ValorJogadorAjustado),
    aplicar_poder(ValorBot, Poder2, Poder1, ValorBotAjustado),
    ( ValorJogadorAjustado > ValorBotAjustado ->
        Vencedor = 1
    ; ValorBotAjustado > ValorJogadorAjustado ->
        Vencedor = 2
    ; Vencedor = 0).

% Aplica o poder à carta
aplicar_poder(Valor, mais_dois, inverte, ValorAjustado) :- ValorAjustado is ( Valor + 2 ) * -1.
aplicar_poder(Valor, menos_dois, inverte, ValorAjustado) :- ValorAjustado is ( Valor + 2 ) * -1.
aplicar_poder(Valor, _, inverte, ValorAjustado) :- ValorAjustado is Valor * -1.
aplicar_poder(Valor, mais_dois, _, ValorAjustado) :- ValorAjustado is Valor + 2.
aplicar_poder(Valor, menos_dois, _, ValorAjustado) :- ValorAjustado is Valor + 2.
aplicar_poder(Valor, inverte, _, ValorAjustado) :- ValorAjustado is Valor * -1.
aplicar_poder(Valor, _, _, Valor) :- !.