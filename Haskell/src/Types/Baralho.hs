module Types.Baralho (
    Baralho,
    gerarCartasElemento,
    novoBaralho,
    pegarCarta,
    inimigoPegarCarta,
    extrairCartas,
    embaralhar
) where

import Types.Carta
import Types.Elemento
import System.Random (randomRIO)

newtype Baralho = Baralho [Carta] deriving (Show, Eq)

gerarCartasElemento :: Elemento -> [Carta]
gerarCartasElemento elemento = 
    [ Carta elemento valor poder | (valor, poder) <- zip [1..12] poderes ]
  where
    poderes = replicate 8 Null ++ [MaisDois, MenosDois, Bloquear elemento, Inverte]

novoBaralho :: Baralho
novoBaralho = Baralho (concatMap gerarCartasElemento [Fogo, Agua, Neve])

pegarCarta :: Baralho -> (Maybe Carta, Baralho)
pegarCarta (Baralho []) = (Nothing, Baralho [])
pegarCarta (Baralho (carta:resto)) = (Just carta, Baralho resto)

inimigoPegarCarta :: Baralho -> Maybe Elemento -> IO (Maybe Carta, Baralho)
inimigoPegarCarta (Baralho []) _ = do
    novoBaralho <- embaralhar novoBaralho
    inimigoPegarCarta novoBaralho Nothing  -- depois de embaralhar, o elemento proibido é ignorado
inimigoPegarCarta baralho@(Baralho (carta@(Carta elemento _ _):resto)) (Just elementoProibido)
    | elemento == elementoProibido = inimigoPegarCarta (Baralho resto) (Just elementoProibido)
    | otherwise = return (Just carta, Baralho resto)
inimigoPegarCarta (Baralho (carta:resto)) Nothing = return (Just carta, Baralho resto)

extrairCartas :: Int -> Baralho -> ([Carta], Baralho)
extrairCartas 0 baralho = ([], baralho)
extrairCartas _ (Baralho []) = ([], Baralho [])
extrairCartas n baralho =
    let (carta, baralhoRestante) = pegarCarta baralho
    in case carta of
        Nothing -> ([], baralhoRestante)
        Just c  -> let (restoCartas, baralhoFinal) = extrairCartas (n - 1) baralhoRestante
                   in (c : restoCartas, baralhoFinal)

embaralhar :: Baralho -> IO Baralho
embaralhar (Baralho []) = return (Baralho [])
embaralhar (Baralho xs) = do
    shuffled <- fisherYates xs
    return (Baralho shuffled)
  where
    fisherYates [] = return []
    fisherYates lst = go (length lst - 1) lst
      where
        go 0 list = return list
        go n list = do
            idx <- randomRIO (0, n)
            let (front, x:back) = splitAt idx list
            shuffledBack <- go (n - 1) (front ++ back)
            return (x : shuffledBack)