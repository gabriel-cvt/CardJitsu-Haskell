module Main (main) where

import Util.Lib
import System.Console.ANSI
import Controllers.ControleSalvamentos

main :: IO ()
main = start

start :: IO()
start = do
    clearScreen
    pressionarTecla
    menu

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
        "3" -> verificarFaixa
    --   "4" -> readFile "./src/Repositories/instrucoes.txt"
        _ -> putStrLn "Opção inválida, escolha novamente!" >> pressionarTecla >> menu