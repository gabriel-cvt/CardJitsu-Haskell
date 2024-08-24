module Controllers.Fases (
    faseInicial,
    faseAzul,
    faseRoxa,
    faseMarrom,
    faseFinal
) where

import System.Console.ANSI

faseInicial :: IO()
faseInicial = do
    clearScreen
    putStrLn "someFunc"

faseAzul :: IO()
faseAzul = putStrLn "someFunc"

faseRoxa :: IO()
faseRoxa = putStrLn "someFunc"

faseMarrom :: IO()
faseMarrom = putStrLn "someFunc"

faseFinal :: IO()
faseFinal = putStrLn "someFunc"