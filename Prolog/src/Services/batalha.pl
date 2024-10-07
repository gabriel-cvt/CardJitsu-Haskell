:- module(batalha, [
    vencedor_rodada/1,
    combate_por_elemento/3,
    combate_por_valor/3,
    combate/3,
    combate_invertido/3,
    combate_por_valor_invertido/3,
    combate_com_poder/5,
    combate_por_valor_com_poder/5
]).

:- use_module('elemento.pl').
:- use_module('carta.pl').

% VencedorRodada
vencedor_rodada(jogador).
vencedor_rodada(adversario).
vencedor_rodada(nenhum).

% combate/3
combate(C1, C2, Vencedor) :-
    combate_por_elemento(C1, C2, ResultadoElemento),
    (ResultadoElemento \= nenhum ->
        Vencedor = ResultadoElemento
    ;
        combate_por_valor(C1, C2, Vencedor)
    ).

% combate_por_elemento/3
combate_por_elemento(carta(ElJogador, _, _), carta(ElBot, _, _), Vencedor) :-
    (prioridade_elemento(ElJogador, ElBot) ->
        Vencedor = jogador
    ; prioridade_elemento(ElBot, ElJogador) ->
        Vencedor = adversario
    ;
        Vencedor = nenhum
    ).

% combate_por_valor/3
combate_por_valor(carta(_, ValorJogador, _), carta(_, ValorBot, _), Vencedor) :-
    (ValorJogador > ValorBot ->
        Vencedor = jogador
    ; ValorBot > ValorJogador ->
        Vencedor = adversario
    ;
        Vencedor = nenhum
    ).

% combate_com_poder/5
combate_com_poder(C1, C2, P1, P2, Vencedor) :-
    combate_por_elemento(C1, C2, ResultadoElemento),
    (ResultadoElemento \= nenhum ->
        Vencedor = ResultadoElemento
    ;
        combate_por_valor_com_poder(C1, C2, P1, P2, Vencedor)
    ).

% combate_invertido/3
combate_invertido(C1, C2, Vencedor) :-
    combate_por_elemento(C1, C2, ResultadoElemento),
    (ResultadoElemento \= nenhum ->
        Vencedor = ResultadoElemento
    ;
        combate_por_valor_invertido(C1, C2, Vencedor)
    ).

% combate_por_valor_invertido/3
combate_por_valor_invertido(carta(_, ValorJogador, _), carta(_, ValorBot, _), Vencedor) :-
    (ValorJogador > ValorBot ->
        Vencedor = adversario
    ; ValorBot > ValorJogador ->
        Vencedor = jogador
    ;
        Vencedor = nenhum
    ).

% combate_por_valor_com_poder/5
combate_por_valor_com_poder(carta(_, ValorJogador, _), carta(_, ValorBot, _), P1, P2, Vencedor) :-
    ajustar_valor(ValorJogador, P1, P2, ValorJogadorAjustado),
    ajustar_valor(ValorBot, P2, P1, ValorBotAjustado),
    (ValorJogadorAjustado > ValorBotAjustado ->
        Vencedor = jogador
    ; ValorBotAjustado > ValorJogadorAjustado ->
        Vencedor = adversario
    ;
        Vencedor = nenhum
    ).

% Helper predicate to adjust values based on power
ajustar_valor(Valor, 2, _, ValorAjustado) :-
    ValorAjustado is Valor + 2.
ajustar_valor(Valor, _, 3, ValorAjustado) :-
    ValorAjustado is Valor - 2.
ajustar_valor(Valor, _, _, Valor).