module Util.Lib (
    pressionarTecla,
    pathPlayer
) where

import GHC.IO.Handle
import System.IO

-- Teste inatividade
pressionarTecla :: IO()
pressionarTecla = do
    putStr "Pressione a tecla ENTER para continuar... \n"
    hFlush stdout
    _ <- getChar
    return()

-- Caminho onde vai salvar o jogador
pathPlayer :: String
pathPlayer = "./src/Repositories/ninja.txt"