module Controllers.ControleJogo (
    menu,
    controllerJogo,
    verificaVencedor,
    jogadorGanhou,
    jogadorPerdeu,
    carregarFase,
    carregarJogo,
    novoJogo
) where

import System.Exit (exitSuccess)
import System.Console.ANSI (clearScreen)
import Util.Lib (pressionarTecla, salvarJogador, carregaJogador, existeProgresso, verificarFaixa, lerInstrucoes, loadingBar)
import Types.Player (upPartidasJogadas, Player, getFaixa, getNome, upFaixa, Faixa (..), newPlayer)
import Services.Jogo (iniciarJogo)
import Control.Concurrent (threadDelay)
import Util.Textos (textoFaseInicial, textoFaseAzul, textoFaseRoxa, textoFaseMarrom, textoFaseFinal, textoZerou)



menu :: IO()
menu = do
    clearScreen
    putStrLn "======= Bem vindo ao Dojo! ======="
    putStrLn "Qual vai ser a sua escolha para hoje?"
    putStrLn "1 - Começar novo jogo"
    putStrLn "2 - Carregar jogo"
    putStrLn "3 - Checar faixas"
    putStrLn "4 - Instruções"
    putStrLn "5 - Sair do jogo"

    
    input <- getLine
    clearScreen
    case input of
        "1" -> novoJogo
        "2" -> carregarJogo
        "3" -> verificarFaixa >> pressionarTecla >> menu
        "4" -> lerInstrucoes >> pressionarTecla >> menu
        "5" -> putStrLn "Até a próxima, jovem ninja" >> exitSuccess
        _ -> putStrLn "Opção inválida, escolha novamente!" >> pressionarTecla >> menu

-- Controller geral do jogo, que leva para a função do jogo, como é uma MD3
-- Obrigatoriamente existirá duas partidas, cada partida retornará o vencedor (PLAYER/BOT)
-- Se for o mesmo vencedor, então ele já ganha e verifica quem venceu, se não vai pra última
controllerJogo :: Player -> String -> IO ()
controllerJogo jogador nomeBot = do
    let jogadorPartida = upPartidasJogadas jogador

    primeiraRodada <- iniciarJogo (0, 0)
    if primeiraRodada then do
            clearScreen
            putStrLn $ nomeBot ++ ": Você ganhou a primeira, mas a próxima irei te destruir"
            mapM_ (\pct -> loadingBar pct >> threadDelay 250000) [0, 10 .. 100] >> putStrLn "\n"
            segundaRodada <- iniciarJogo (1, 0)
            if primeiraRodada == segundaRodada then verificaVencedor primeiraRodada jogadorPartida nomeBot
            else do
                clearScreen
                putStrLn $ "Pelo visto a batalha está difícil com " ++ nomeBot ++ ", será necessário um desempate!"
                mapM_ (\pct -> loadingBar pct >> threadDelay 250000) [0, 10 .. 100] >> putStrLn "\n"
                ultimaRodada <- iniciarJogo (1, 1)
                verificaVencedor ultimaRodada jogadorPartida nomeBot
    else do
        clearScreen
        putStrLn (nomeBot ++ ": Você está levando isso a sério? Dessa forma nunca conseguirá ser um Mestre Ninja")
        mapM_ (\pct -> loadingBar pct >> threadDelay 250000) [0, 10 .. 100] >> putStrLn "\n"
        segundaRodada <- iniciarJogo (0, 1)
        if primeiraRodada == segundaRodada then verificaVencedor primeiraRodada jogadorPartida nomeBot
        else do
            clearScreen
            putStrLn $ "Pelo visto a batalha está difícil com " ++ nomeBot ++ ", será necessário um desempate!"
            mapM_ (\pct -> loadingBar pct >> threadDelay 250000) [0, 10 .. 100] >> putStrLn "\n"
            ultimaRodada <- iniciarJogo (1, 1)
            verificaVencedor ultimaRodada jogadorPartida nomeBot

-- Verifica quem foi o vencedor da partida, utiliza a data VencedorPartida
verificaVencedor :: Bool -> Player -> String -> IO()
verificaVencedor jogadorVenceu jogador nomeBot = do
    if jogadorVenceu then jogadorGanhou jogador nomeBot 
    else jogadorPerdeu nomeBot


-- Quando o jogo ganha, ele será recompensado upando de faixa e passará para próxima fase
jogadorGanhou :: Player -> String -> IO ()
jogadorGanhou jogador nomeBot = do
    putStrLn $ "Parabéns pela vitória, " ++ getNome jogador

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
    putStrLn "Deseja continuar jogando ou voltar ao Dojo?"
    putStrLn "1 - Continuar Jogando"
    putStrLn "2 - Voltar ao Dojo"
    input <- getLine
    case input of
        "1" -> carregarFaseJogador
        "2" -> menu
        _ -> putStrLn "Opção inválida, escolha novamente!" >> pressionarTecla >> continuarJogando

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

-- Função que leva para o carregamento da fase na função mais específica carregarFaseJogador
carregarFase :: IO()
carregarFase = do
    clearScreen
    putStrLn "Carregando fase..."
    mapM_ (\pct -> loadingBar pct >> threadDelay 400000) [0, 10 .. 100] >> putStrLn "\n"
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
        _ -> zerouJogo

faseInicial :: IO()
faseInicial = do
    clearScreen
    jogador <- carregaJogador
    textoFaseInicial
    pressionarTecla
    controllerJogo jogador "O Ruivo"

faseAzul :: IO()
faseAzul = do
    clearScreen
    jogador <- carregaJogador
    textoFaseAzul
    pressionarTecla
    controllerJogo jogador "Bruxa da Neve"

faseRoxa :: IO()
faseRoxa = do
    clearScreen
    jogador <- carregaJogador
    textoFaseRoxa jogador
    pressionarTecla
    controllerJogo jogador "Cavaleiro do Mar"

faseMarrom :: IO()
faseMarrom = do
    clearScreen
    jogador <- carregaJogador
    textoFaseMarrom jogador
    pressionarTecla
    controllerJogo jogador "Punhos de Fogo"

faseFinal :: IO()
faseFinal = do
    clearScreen
    jogador <- carregaJogador
    textoFaseFinal jogador
    pressionarTecla
    controllerJogo jogador "Olhos de Falcão"
    zerouJogo

zerouJogo :: IO ()
zerouJogo = do
    clearScreen
    jogador <- carregaJogador
    textoZerou jogador
    pressionarTecla
    putStrLn "1 - Abandonar tudo que você conseguiu até agora e começar uma nova Jornada"
    putStrLn "2 - Finalizar o jogo como um grande ninja"
    input <- getLine
    case input of
        "1" -> do
            clearScreen
            putStrLn "Então você vai abandonar tudo que conseguiu e ir atrás da conquista novamente..." >> pressionarTecla >> novoJogo
        "2" -> do
            putStrLn "Bem, o caminho que você fez não foi fácil, e após tantas batalhas, chegou o momento de descansar" >> pressionarTecla >>exitSuccess
        _ -> putStrLn "Opção inválida, escolha novamente!" >> pressionarTecla >> continuarJogando