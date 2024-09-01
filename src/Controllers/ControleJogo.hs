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



menu :: IO()
menu = do
    clearScreen
    putStrLn "======= Bem vindo ao Dojo! ======="
    putStrLn "Deseja carregar um jogo ou começar um novo?"
    putStrLn "1 - Começar novo jogo"
    putStrLn "2 - Carregar jogo"
    putStrLn "3 - Checar faixas"
    putStrLn "4 - Instruções"

    
    input <- getLine
    clearScreen
    case input of
        "1" -> novoJogo
        "2" -> carregarJogo
        "3" -> verificarFaixa >> pressionarTecla >> menu
        "4" -> lerInstrucoes >> pressionarTecla >> menu
        _ -> putStrLn "Opção inválida, escolha novamente!" >> pressionarTecla >> menu

-- Controller geral do jogo, que leva para a função do jogo, como é uma MD3
-- Obrigatoriamente existirá duas partidas, cada partida retornará o vencedor (PLAYER/BOT)
-- Se for o mesmo vencedor, então ele já ganha e verifica quem venceu, se não vai pra última
controllerJogo :: Player -> String -> IO()
controllerJogo jogador nomeBot = do
    let jogadorPartida = upPartidasJogadas jogador

    primeiraRodada <-  iniciarJogo

    segundaRodada <- iniciarJogo

    if primeiraRodada == segundaRodada
    then verificaVencedor primeiraRodada jogadorPartida nomeBot

    else do
        putStrLn $ "Pelo visto a batalha está dificíl com " ++ nomeBot ++ ", será necessário um desempate!"
        ultimaRodada <- iniciarJogo
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
    mapM_ (\pct -> loadingBar pct >> (threadDelay 500000)) [0, 10 .. 100] >> putStrLn "\n"
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
    putStrLn "Nessa fase, você se encontra com o famoso Ruivo, ele vai ter ensinar o funcionamento do jogo"
    pressionarTecla
    controllerJogo jogador "O Ruivo"

faseAzul :: IO()
faseAzul = do
    clearScreen
    jogador <- carregaJogador
    putStrLn "A bruxa da neve se aproxima, ela é especialista em cartas de neve!"
    putStrLn "Não restam opções para subir de faixa a não ser derrotando ela!\n"
    pressionarTecla
    controllerJogo jogador "Bruxa da Neve"

faseRoxa :: IO()
faseRoxa = do
    clearScreen
    jogador <- carregaJogador
    putStrLn "O cavaleiro do mar, que suspeitava da queda da Bruxa, já foi perguntando como você fez esse feito"
    putStrLn "Você respondeu que somente batalhando para saber"
    putStrLn $ "Então é assim, " ++ getNome jogador ++ "? Então É HORA DO DUELO!\n"
    pressionarTecla
    controllerJogo jogador "Cavaleiro do Mar"

faseMarrom :: IO()
faseMarrom = do
    clearScreen
    jogador <- carregaJogador
    putStrLn "Punhos de fogo apareceu para vingar seu amigo, o cavaleiro"
    putStrLn "ELE ESTÁ FURIOSO!! TUDO QUE ELE VÊ PELA FRENTE ESTÁ PEGANDO FOGO!!"
    putStrLn $ getNome jogador ++ ", como você ganhará dele??\n"
    pressionarTecla
    controllerJogo jogador "Punhos de Fogo"

faseFinal :: IO()
faseFinal = do
    clearScreen
    jogador <- carregaJogador
    putStrLn "Olhos de Falcão, após ver todos os seus aprendizes derrotados.."
    putStrLn "não vê outra opção, a não ser lhe mostrar o que é pedir misericórdia"
    putStrLn $ "Você não vê... " ++ getNome jogador ++ ", o mundo é vasto..."
    putStrLn "Preocupado com uma única folha, você não verá a árvore."
    putStrLn "Preocupado com uma única árvore você não perceberá toda a floresta. Não se preocupe com um único ponto."
    putStrLn "Veja tudo em sua plenitude sem se esforçar."
    putStrLn "Vamos... vamos ao duelo!\n"
    pressionarTecla
    controllerJogo jogador "Olhos de Falcão"
    zerouJogo

zerouJogo :: IO ()
zerouJogo = do
    clearScreen
    jogador <- carregaJogador
    putStrLn "Após derrotar o grande Sensei, você conseguiu alcançar algo que todos desejam..."
    putStrLn "Você encontrou finalmente o One pi..."
    putStrLn "A GRANDE MÁSCARA NINJA!"
    putStrLn ("Após você conquistar esse feito, só restam duas opções, " ++ getNome jogador ++ "...")
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