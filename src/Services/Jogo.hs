module Services.Jogo (
    jogo,
    VencedorPartida (..)
) where
import Types.Player (Player)

data VencedorPartida = PLAYER | BOT deriving (Eq, Show)

-- Função criada apenas pra rodar o build, ainda não feita
jogo :: Player -> String -> VencedorPartida
jogo jogador nomeBot = PLAYER