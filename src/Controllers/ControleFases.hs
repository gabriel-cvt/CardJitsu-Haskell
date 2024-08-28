module Controllers.ControleFases (
    carregarFaseJogador,
    carregarFase,
    faseInicial,
    faseAzul,
    faseRoxa,
    faseMarrom,
    faseFinal
) where

import Util.Lib
import System.Console.ANSI
import Types.Player

-- Função que leva para o carregamento da fase na função mais específica carregarFaseJogador
carregarFase :: IO()
carregarFase = do
    putStrLn "Carregando fase..."
    pressionarTecla
    clearScreen
    carregarFaseJogador

-- Carrega a fase do jogador de acordo com a faixa dele
-- Funções de fases ainda não criadas
carregarFaseJogador :: IO()
carregarFaseJogador = do
    jogador <- carregaJogador
    let faixa = getFaixa jogador
    case faixa of
        Branca -> faseInicial
        Azul -> faseAzul
        Roxa -> faseRoxa
        Marrom -> faseMarrom
        Preta -> faseFinal

faseInicial :: IO()
faseInicial = do
    clearScreen
    putStrLn "someFunc"

faseAzul :: IO()
faseAzul = putStrLn "someFunc"

faseRoxa :: IO()
faseRoxa = putStrLn "someFunc"

faseMarrom :: IO()
faseMarrom = putStrLn "someFunc"

faseFinal :: IO()
faseFinal = putStrLn "someFunc"