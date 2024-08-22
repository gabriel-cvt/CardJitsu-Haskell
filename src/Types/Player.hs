module Types.Player (
    Player (..),
    getNome,
    getFaixa,
    upFaixa,
    newPlayer,
    getProgresso
) where
import Types.Carta
import Types.Faixa

-- Data do jogador
-- String: Nome
-- [Carta]: deck de cartas
-- Faixa: cor da faixa/progresso do jogador
-- Int: Partidas jogadas
-- Int: Progresso inicial
data Player = Player String [Carta] Faixa Int Int deriving (Show, Read)


-- pegar o nome do jogador
getNome:: Player -> String
getNome (Player nomePlayer _ _ _ _) = nomePlayer

-- pegar a faixa atual do jogador
getFaixa:: Player -> Faixa
getFaixa (Player _ _ faixaPlayer _ _) = faixaPlayer

getProgresso:: Player -> Int
getProgresso (Player _ _ _ _ progresso) = progresso

-- Método para aumentar a faixa do jogador
upFaixa:: Player -> Player 
upFaixa (Player nomePlayer cartas faixaPlayer partidas progresso) = Player nomePlayer cartas (succ faixaPlayer) partidas progresso

newPlayer :: String -> Player
newPlayer nome = Player nome [] Branca 0 0