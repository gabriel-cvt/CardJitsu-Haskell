module Types.Player (
    Player (..),
    Faixa (..),
    getNome,
    getFaixa,
    upFaixa,
    upPartidasJogadas,
    newPlayer,
    getProgresso
) where
import Types.Carta

-- Data do jogador
-- String: Nome
-- [Carta]: deck de cartas
-- Faixa: cor da faixa/progresso do jogador
-- Int: Partidas jogadas
-- Int: Progresso inicial
data Player = Player String [Carta] Faixa Int deriving (Show, Read)
data Faixa = Branca | Azul | Roxa | Marrom | Preta | Mestre deriving (Eq, Show, Enum, Read)

-- pegar o nome do jogador
getNome:: Player -> String
getNome (Player nomePlayer _ _ _ ) = nomePlayer

-- pegar a faixa atual do jogador
getFaixa:: Player -> Faixa
getFaixa (Player _ _ faixaPlayer _ ) = faixaPlayer

getProgresso:: Player -> Int
getProgresso (Player _ _ _  progresso) = progresso

-- Método para aumentar a faixa do jogador
upFaixa:: Player -> Player 
upFaixa (Player nomePlayer cartas faixaPlayer partidas) = Player nomePlayer cartas (succ faixaPlayer) partidas

upPartidasJogadas :: Player -> Player
upPartidasJogadas (Player nomePlayer cartas faixaPlayer partidas) = Player nomePlayer cartas faixaPlayer (partidas + 1)

newPlayer :: String -> Player
newPlayer nome = Player nome [] Branca 0 