module Controllers.ControleJogo (
    controllerJogo,
    verificaVencedor,
    jogadorGanhou,
    jogadorPerdeu,
) where

import System.Exit (exitSuccess)
import System.Console.ANSI (clearScreen)
import Controllers.ControleFases (carregarFaseJogador)
import Util.Lib (pressionarTecla, salvarJogador)
import Types.Player (upPartidasJogadas, Player, getFaixa, getNome, upFaixa)
import Services.Jogo (iniciarJogo)

-- Controller geral do jogo, que leva para a função do jogo, como é uma MD3
-- Obrigatoriamente existirá duas partidas, cada partida retornará o vencedor (PLAYER/BOT)
-- Se for o mesmo vencedor, então ele já ganha e verifica quem venceu, se não vai pra última
controllerJogo :: Player -> String -> IO()
controllerJogo jogador nomeBot = do
    let jogadorPartida = upPartidasJogadas jogador

    let primeiraRodada =  iniciarJogo

    let segundaRodada = iniciarJogo

    if primeiraRodada == segundaRodada
    then verificaVencedor primeiraRodada jogadorPartida nomeBot

    else do
        putStrLn $ "Pelo visto a batalha está dificíl com " ++ nomeBot ++ ", será necessário um desempate!"
        let ultimaRodada = iniciarJogo
        verificaVencedor ultimaRodada jogadorPartida nomeBot

-- Verifica quem foi o vencedor da partida, utiliza a data VencedorPartida
verificaVencedor :: Bool -> Player -> String -> IO()
verificaVencedor jogadorVenceu jogador nomeBot = do
    if jogadorVenceu then jogadorGanhou jogador nomeBot 
    else jogadorPerdeu nomeBot


-- Quando o jogo ganha, ele será recompensado upando de faixa e passará para próxima fase
jogadorGanhou :: Player -> String -> IO ()
jogadorGanhou jogador nomeBot = do
    putStrLn $ "Parabéns pela vitória, " ++ getNome jogador ++"\n"

    let faixaAtual = getFaixa jogador
    putStrLn $ "Após derrotar " ++ nomeBot ++ " bravamente, você conquistou a faixa " ++ show (succ faixaAtual) ++ "!!\n"
    let jogadorAtualizado = upFaixa jogador

    salvarJogador jogadorAtualizado
    pressionarTecla
    continuarJogando

-- Quando o jogador perde a partida para o BOT, a 2 opção deve ser Voltar para o DOJO, mas não consegui
-- resolver o erro de import cíclico, então deixei assim e vou resolver depois
jogadorPerdeu :: String -> IO ()
jogadorPerdeu nomeBot = do
    clearScreen
    putStrLn $ nomeBot ++ ": " ++ "Não foi dessa vez, melhore se quiser me desafiar novamente..."
    pressionarTecla
    continuarJogando

continuarJogando :: IO ()
continuarJogando = do
    clearScreen
    putStrLn "Deseja continuar jogando ou sair do jogo?"
    putStrLn "1 - Continuar Jogando"
    putStrLn "2 - Sair do Jogo"
    input <- getLine
    case input of
        "1" -> carregarFaseJogador
        "2" -> do
            putStrLn "Bem, então até a próxima vez, Ninja!"
            exitSuccess
        _ -> putStrLn "Opção inválida, escolha novamente!" >> pressionarTecla >> continuarJogando