module Services.Batalha (
    VencedorRodada (..),
    combatePorElemento,
    combatePorValor,
    combate,
    combateInvertido,
    combatePorValorInvertido,
    combateComPoder,
    combatePorValorComPoder

) where
    
import Types.Carta
import Types.Elemento

data VencedorRodada = JOGADOR | ADVERSARIO | NENHUM deriving (Eq, Show)

combate :: Carta -> Carta -> VencedorRodada
combate c1 c2 =
    case combatePorElemento c1 c2 of
        JOGADOR -> JOGADOR
        ADVERSARIO -> ADVERSARIO
        NENHUM -> combatePorValor c1 c2

combatePorElemento:: Carta -> Carta -> VencedorRodada
combatePorElemento (Carta elJogador _ _ ) (Carta elBot _ _ )
    | prioridadeElemento elJogador elBot = JOGADOR
    | prioridadeElemento elBot elJogador = ADVERSARIO
    | otherwise = NENHUM

combatePorValor:: Carta -> Carta -> VencedorRodada
combatePorValor (Carta _ valorJogador _) (Carta _ valorBot _)
    | valorJogador > valorBot = JOGADOR
    | valorBot > valorJogador = ADVERSARIO
    | otherwise = NENHUM

combateComPoder :: Carta -> Carta -> Int -> Int -> VencedorRodada
combateComPoder c1 c2 p1 p2 =
    case combatePorElemento c1 c2 of
	JOGADOR -> JOGADOR
 	ADVERSARIO -> ADVERSARIO
	NENHUM -> combatePorValorComPoder c1 c2 p1 p2

combateInvertido :: Carta -> Carta -> VencedorRodada
combateInvertido c1 c2 =
    case combatePorElemento c1 c2 of
        JOGADOR -> JOGADOR
        ADVERSARIO -> ADVERSARIO
        NENHUM -> combatePorValorInvertido c1 c2

combatePorValorInvertido :: Carta -> Carta -> VencedorRodada
combatePorValorInvertido (Carta _ valorJogador _) (Carta _ valorBot _)
    | valorJogador > valorBot = ADVERSARIO
    | valorBot > valorJogador = JOGADOR
    | otherwise = NENHUM

combatePorValorComPoder :: Carta -> Carta -> Int -> Int -> VencedorRodada
combatePorValorComPoder (Carta _ valorJogador _) (Carta _ valorBot _) p1 p2
    | valorJogadorAjustado > valorBotAjustado = JOGADOR
    | valorBotAjustado > valorJogadorAjustado = ADVERSARIO
    | otherwise = NENHUM
    where
    valorJogadorAjustado
        | p1 == 2 = valorJogador + 2
        | p2 == 3 = valorJogador - 2
        | otherwise = valorJogador
    valorBotAjustado
        | p1 == 3 = valorBot - 2
        | p2 == 2 = valorBot + 2
        | otherwise = valorBot
