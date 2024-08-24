module Controllers.ControleSalvamentos (
    novoJogo,
    inicializaJogador,
    carregarJogo,
    verificarFaixa
) where

import Types.Player
import Services.Salvamentos
import Util.Lib
import System.Console.ANSI
import Controllers.ControleFases
import Types.Faixa


-- Começa um novo jogo, inicializa e guarda o jogador e leva para a fase inicial
-- Função de fase inicial ainda não criada
novoJogo :: IO()
novoJogo = do
    putStrLn "Começando uma nova aventura ninja..."
    putStrLn "Qual é o seu nome?"
    name <- getLine
    inicializaJogador name
    carregarFase

-- Cria o jogador e salva ele pela função salvarJogador
inicializaJogador :: String -> IO()
inicializaJogador nomeJogador = do
    let jogador = newPlayer nomeJogador
    salvarJogador jogador

-- Entrar em um jogo já existente
-- Se existir um progresso, ele irá carregar fase, se não, vai levar para um novo jogo
carregarJogo :: IO()
carregarJogo = do
    putStrLn "Verificando seu progresso..."
    player <- carregaJogador
    if existeProgresso player then carregarFase
    else do
        putStrLn "Você não possui nenhum jogo salvo, então vamos começar um novo agora!"
        pressionarTecla
        clearScreen
        novoJogo

-- Função para checar a faixa do jogador
verificarFaixa :: IO()
verificarFaixa = do
    jogador <- carregaJogador
    let faixaAtual = getFaixa jogador
    let faixasAnteriores = enumFromTo Branca faixaAtual 
    let faixasRestantes = enumFrom (succ faixaAtual)

    case faixaAtual of
            Branca -> do
                putStrLn "Você ainda não alcançou outras faixas. Vamos começar a sua jornada ninja!"
                putStrLn "Faixas anteriores:"
                putStrLn " ➤ Nenhuma"
                putStrLn "Faixas restantes:"
                mapM_ (putStrLn . (" ➤ " ++) . show) faixasRestantes

            Preta -> do
                putStrLn "Parabéns! Você conquistou a faixa preta!"
                putStrLn "Faixas anteriores:"
                mapM_ (putStrLn . (" ➤ " ++) . show) faixasAnteriores
                putStrLn "Faixas restantes:"
                putStrLn " ➤ Nenhuma"

            _ -> do
                putStrLn $ "Você está na faixa: " ++ show faixaAtual
            
                putStrLn "Faixas anteriores:"
                mapM_ (putStrLn . (" ➤ " ++) . show) faixasAnteriores

                putStrLn "Faixas restantes:"
                mapM_ (putStrLn . (" ➤ " ++) . show) faixasRestantes