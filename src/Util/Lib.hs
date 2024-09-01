module Util.Lib (
    pressionarTecla,
    pathPlayer,
    lerInstrucoes,
    salvarJogador,
    carregaJogador,
    existeProgresso,
    verificarFaixa,
    pausarJogo,
    loadingBar,
) where

import GHC.IO.Handle
import System.IO
import Types.Player (getFaixa, Player (..), Faixa (..))

-- Teste inatividade
pressionarTecla :: IO()
pressionarTecla = do
    putStr "Pressione a tecla ENTER para continuar... \n"
    hFlush stdout
    _ <- getChar
    return()

pausarJogo :: IO()
pausarJogo = do
    putStrLn "JOGO PAUSADO!"
    putStrLn "Para despausar, aperte a tecla ENTER"
    hFlush stdout
    _ <- getChar
    return()

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
existeProgresso (Player _ _ _  progresso) = progresso /= 0

-- Caminho onde vai salvar o jogador
pathPlayer :: String
pathPlayer = "./src/Repositories/ninja.txt"

loadingBar :: Int -> IO ()
loadingBar pct = do
    let width = 40  -- Largura total da barra de progresso
    let filled = (pct * (width - 7)) `div` 100  -- Calcula quantos caracteres devem ser preenchidos (reservando espaço para a porcentagem)
    let empty = (width - 7) - filled  -- Calcula o espaço vazio
    
    let bar = replicate filled '#' ++ replicate empty '-'
    let progress = show pct ++ "%"  -- Porcentagem como string

    -- Atualiza a barra de progresso na mesma linha
    putStr $ "\r[" ++ bar ++ "] " ++ progress
    -- Flush the output buffer
    hFlush stdout

-- Lê as instruções do jogo
lerInstrucoes :: IO()
lerInstrucoes = do 
    conteudo <- readFile "./src/Repositories/instrucoes.txt"
    putStrLn conteudo

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
                putStrLn "Objetivo restante:"
                putStrLn " ➤ Desafiar o sensei e virar um Mestre ninja"

            Mestre -> do
                putStrLn "Parabéns! Você é um grande mestre Ninja"
                putStrLn "Faixas anteriores:"
                mapM_ (putStrLn . (" ➤ " ++) . show) faixasAnteriores
                
            _ -> do
                putStrLn $ "Você está na faixa: " ++ show faixaAtual
            
                putStrLn "Faixas anteriores:"
                mapM_ (putStrLn . (" ➤ " ++) . show) faixasAnteriores

                putStrLn "Faixas restantes:"
                mapM_ (putStrLn . (" ➤ " ++) . show) faixasRestantes