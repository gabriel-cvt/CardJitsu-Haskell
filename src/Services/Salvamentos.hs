module Services.Salvamentos (
    salvarJogador,
    carregaJogador,
    existeProgresso,
    carregarFaseJogador
) where

import Types.Player
import System.IO
import Util.Lib
import Types.Faixa
import Controllers.ControleFases


-- Salva progresso do jogador no local de arquivo
salvarJogador :: Player -> IO()
salvarJogador jogador = writeFile pathPlayer (show jogador)

-- Retorna o progresso do jogador que está no local do arquivo
carregaJogador :: IO Player
carregaJogador = do
    handle <- openFile pathPlayer ReadMode
    content <- hGetContents' handle
    hClose handle
    return (read content :: Player)

-- Retorna true para não existir progresso do jogador (progresso == 0)
-- Falso para caso exista um progresso
existeProgresso :: Player -> Bool
existeProgresso (Player _ _ _ _ progresso) = progresso /= 0

-- Carrega a fase do jogador de acordo com a faixa dele
-- Funções de fases ainda não criadas
carregarFaseJogador :: IO()
carregarFaseJogador = do
    jogador <- carregaJogador
    let faixa = getFaixa jogador
    case faixa of
        Branca -> faseInicial
        Azul -> faseDois
        Roxa -> faseTres
        Marrom -> faseQuatro
        Preta -> faseFinal