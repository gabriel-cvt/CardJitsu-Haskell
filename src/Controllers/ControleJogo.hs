module Controllers.ControleJogo (
    controllerJogo,
    verificaVencedor,
    jogadorGanhou,
    passarFaseJogador,
    jogadorPerdeu
) where

import Types.Player
import Services.Salvamentos
import Util.Lib
import System.Console.ANSI
import Services.Jogo
import Controllers.ControleFases
import System.Exit (exitSuccess)

-- Controller geral do jogo, que leva para a função do jogo, como é uma MD3
-- Obrigatoriamente existirá duas partidas, cada partida retornará o vencedor (PLAYER/BOT)
-- Se for o mesmo vencedor, então ele já ganha e verifica quem venceu, se não vai pra última
controllerJogo :: Player -> String -> IO()
controllerJogo jogador nomeBot = do
    let primeiraRodada = jogo jogador nomeBot
    let segundaRodada = jogo jogador nomeBot
    if primeiraRodada == segundaRodada
    then verificaVencedor primeiraRodada nomeBot jogador
    else do
        putStrLn "Pelo visto a batalha está dificíl, será necessário um desempate!"
        let ultimaRodada = jogo jogador nomeBot
        verificaVencedor ultimaRodada nomeBot jogador

-- Verifica quem foi o vencedor da partida, utiliza a data VencedorPartida
verificaVencedor :: VencedorPartida -> String -> Player -> IO()
verificaVencedor vencedor nomeBot jogador = do
    let jogadorPartidaIncrementada = upPartidasJogadas jogador
    if vencedor == PLAYER then jogadorGanhou jogadorPartidaIncrementada nomeBot
    else jogadorPerdeu jogadorPartidaIncrementada nomeBot


-- Quando o jogo ganha, ele será recompensado upando de faixa e passará para próxima fase
jogadorGanhou :: Player -> String -> IO ()
jogadorGanhou jogador nomeBot = do
    putStrLn "Parabéns pela vitória, ninja\n"
    let faixaAtual = getFaixa jogador
    putStrLn ("Após derrotar " ++ nomeBot ++ " bravamente, você conquistou a faixa " ++ show (succ faixaAtual) ++ "!! \n")
    let jogadorAtualizado = upFaixa jogador
    salvarJogador jogadorAtualizado
    passarFaseJogador

-- Quando o jogador perde a partida para o BOT, a 2 opção deve ser Voltar para o DOJO, mas não consegui
-- resolver o erro de import cíclico, então deixei assim e vou resolver depois
jogadorPerdeu :: Player -> String -> IO ()
jogadorPerdeu jogador nomeBot = do
    clearScreen
    putStrLn (nomeBot ++ ": " ++ "Não foi dessa vez, " ++ getNome jogador ++ "...")
    putStrLn "Deseja uma revanche ou sair do jogo?"
    putStrLn ("1 - Revanche contra " ++ nomeBot)
    putStrLn "2 - Ir para o menu principal"
    input <- getLine
    case input of
        "1" -> carregarFaseJogador
        "2" -> exitSuccess
        _ -> putStrLn "Opção inválida, escolha novamente!" >> pressionarTecla >> jogadorPerdeu jogador nomeBot