module Types.Deque (
    Deque,
    criarDeque,
    completarDeque,
    jogar
) where

import Types.Carta
import Types.Baralho

newtype Deque = Deque [Carta] deriving (Eq)

criarDeque :: Baralho -> (Deque, Baralho)
criarDeque baralhoInicial = 
    let (cartasIniciais, novoBaralho) = extrairCartas 5 baralhoInicial
    in (Deque cartasIniciais, novoBaralho)

completarDeque :: Deque -> Baralho -> (Deque, Baralho)
completarDeque (Deque cartas) baralhoAtual =
    let (carta, novoBaralho) = pegarCarta baralhoAtual
    in case carta of
        Nothing -> (Deque cartas, novoBaralho)
        Just c  -> (Deque (cartas ++ [c]), novoBaralho)

jogar :: Int -> Deque -> (Maybe Carta, Deque)
jogar escolha (Deque cartas)
    | escolha >= 0 && escolha < length cartas = 
        let (antes, carta:depois) = splitAt escolha cartas
        in (Just carta, Deque (antes ++ depois))
    | otherwise = (Nothing, Deque cartas)

instance Show Deque where
    show (Deque cartas) = 
        "DEQUE :\n" ++ unlines (zipWith formatCard [1..] cartas)
      where
        formatCard :: Int -> Carta -> String
        formatCard i carta = "[" ++ show i ++ "] - " ++ show carta