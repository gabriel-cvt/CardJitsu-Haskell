module Services.Batalha (
    combatePorElemento,
    combatePorValor,
    aplicarMaisDois,
    aplicarMenosDois,
    VencedorRodada (..)
) where
    
import Types.Carta
import Types.Elemento

data VencedorRodada = JOGADOR | ADVERSARIO | NENHUM deriving (Eq, Show)

-- Funções de cartas em Batalha!
-- OBS: Acesso de valores feitos por pattern matching

-- Saída não terminada
combatePorElemento:: Carta -> Carta -> VencedorRodada
combatePorElemento (Carta elJogador vJogador _ ) (Carta elBot vBot _ )
    | prioridadeElemento elJogador elBot = JOGADOR
    | prioridadeElemento elBot elJogador = ADVERSARIO
    | otherwise = combatePorValor vJogador vBot

-- Saída não terminada
combatePorValor:: Int -> Int -> VencedorRodada
combatePorValor valorJogador valorBot
    | valorJogador > valorBot = JOGADOR
    | valorBot > valorJogador = ADVERSARIO
    | otherwise = NENHUM

-- Aplicar poder na carta

aplicarMaisDois:: Carta -> Carta
aplicarMaisDois (Carta elemento valor poder) = Carta elemento (valor + 2) poder

aplicarMenosDois:: Carta -> Carta
aplicarMenosDois (Carta elemento valor poder) = Carta elemento (valor - 2) poder