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

controllerJogo :: Player -> String -> IO()
controllerJogo jogador nomeBot = do
    let primeiroVencedor = jogo jogador nomeBot
    let segundoVencedor = jogo jogador nomeBot
    if primeiroVencedor == segundoVencedor
    then verificaVencedor primeiroVencedor nomeBot jogador
    else do
        putStrLn "Pelo visto a batalha está dificíl, será necessário um desempate!"
        let ultimaRodada = jogo jogador nomeBot
        verificaVencedor ultimaRodada nomeBot jogador

verificaVencedor :: VencedorPartida -> String -> Player -> IO()
verificaVencedor vencedor nomeBot jogador = do
    if vencedor == PLAYER then jogadorGanhou jogador nomeBot
    else jogadorPerdeu jogador nomeBot

jogadorGanhou :: Player -> String -> IO ()
jogadorGanhou jogador nomeBot = do
    putStrLn "Parabéns pela vitória, ninja\n"
    let faixaAtual = getFaixa jogador
    putStrLn ("Após derrotar " ++ nomeBot ++ " bravamente, você conquistou a faixa " ++ show (succ faixaAtual) ++ "!! \n")
    let player = upFaixa jogador
    salvarJogador player
    passarFaseJogador

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