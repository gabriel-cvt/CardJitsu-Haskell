 module([get_faixa/2, up_faixa/2]).

:- user_module('src/Types/player.pl')

faixa(branca).
faixa(azul).
faixa(roxa).
faixa(marrom).
faixa(preta).
faixa(mestre).

faixa_sucessora(branca, azul).
faixa_sucessora(azul, roxa).
faixa_sucessora(roxa, marrom).
faixa_sucessora(marrom, preta).
faixa_sucessora(preta, mestre).

get_faixa(player(_, Faixa, _), Faixa).

up_faixa(player(Nome, FaixaAtual, Progresso), player(Nome, NovaFaixa, Progresso)) :-
    faixa_sucessora(FaixaAtual, NovaFaixa).
