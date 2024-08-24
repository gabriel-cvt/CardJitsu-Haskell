module Controllers.ControleFases (
    passarFaseJogador,
    carregarFaseJogador,
    carregarFase
) where

import Util.Lib
import System.Console.ANSI
import Types.Faixa
import Services.Salvamentos
import Types.Player
import Controllers.Fases
import System.Exit (exitSuccess)

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

-- Leva o jogador para a próxima fase ou ir para o dojo, como eu ainda não resolvi o problema
-- de importação cíclica, botei pra sair do jogo, mas ainda vou tentar mudar
passarFaseJogador :: IO ()
passarFaseJogador = do
    clearScreen
    putStrLn "Deseja avançar para o próximo desafio ou sair do jogo?"
    putStrLn "1 - Próxima fase"
    putStrLn "2 - Ir para o menu principal"
    input <- getLine
    case input of
        "1" -> carregarFaseJogador
        "2" -> exitSuccess
        _ -> putStrLn "Opção inválida, escolha novamente!" >> pressionarTecla >> passarFaseJogador