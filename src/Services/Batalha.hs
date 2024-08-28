module Services.Batalha (
    VencedorRodada (..),
    combatePorElemento,
    combatePorValor,
    combate
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