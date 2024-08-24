module Types.Baralho (
    baralho,
    embaralhar
) where

import Types.Carta
import Types.Elemento
import System.Random (randomRIO)

gerarCartasElemento :: Elemento -> [Carta]
gerarCartasElemento elemento =
    [Carta elemento valor poder | (valor, poder) <- zip [1..12] poderes]
    where
        poderes = replicate 12 Null

baralho :: [Carta]
baralho = gerarCartasElemento Fogo ++ gerarCartasElemento Agua ++ gerarCartasElemento Neve

-- Embaralha uma lista usando o algoritmo de Fisher-Yates
embaralhar :: [a] -> IO [a]
embaralhar [] = return []
embaralhar xs = go (length xs - 1) xs
  where
    go 0 list = return list
    go n list = do
        idx <- randomRIO (0, n)
        let (front, x:back) = splitAt idx list
        shuffledBack <- go (n - 1) (front ++ back)
        return (x : shuffledBack)
