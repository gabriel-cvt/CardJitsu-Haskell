module Main (main) where

import System.Console.ANSI
import Controllers.ControleJogo (menu)
import Util.Lib (pressionarTecla)

main :: IO ()
main = start

start :: IO()
start = do
    clearScreen
    pressionarTecla
    menu