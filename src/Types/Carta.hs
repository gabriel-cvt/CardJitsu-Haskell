module Types.Carta (
    Carta(..),
    Poder(..),
    aplicarPoder,
    atualizarPilhaPoder,
    limitarValor
) where

import Types.Elemento

data Carta = Carta Elemento Int Poder deriving (Eq, Read)
data Poder = Bloquear Elemento | MaisDois | MenosDois | Inverte | Null deriving (Eq, Read)

instance Show Poder where
    show (Bloquear elemento) = "Bloquear " ++ show elemento
    show MaisDois = "Mais Dois"
    show MenosDois = "Menos Dois"
    show Inverte = "Inverte"
    show Null = "Nenhum"

instance Show Carta where
    show (Carta elemento valor poder) =
        case poder of
            Null -> "( " ++ show elemento ++ " : " ++ show valor ++ " )"
            _    -> "( " ++ show elemento ++ " : " ++ show valor ++ " -> " ++ show poder ++ " )"

-- Aplica o poder da carta e retorna uma nova carta com o efeito do poder
aplicarPoder :: Poder -> Carta -> Carta
aplicarPoder Null carta = carta
aplicarPoder MaisDois (Carta elemento valor poder) = Carta elemento (limitarValor (valor + 2)) poder
aplicarPoder MenosDois (Carta elemento valor poder) = Carta elemento (limitarValor (valor - 2)) poder
aplicarPoder Inverte (Carta elemento valor poder) = Carta elemento (valor * (-1)) poder
aplicarPoder (Bloquear _) carta = carta

-- Atualiza a pilha de poderes com o poder da carta jogada
atualizarPilhaPoder :: Carta -> [Poder] -> [Poder]
atualizarPilhaPoder (Carta _ _ poder) pilha = poder : pilha

-- Limita o valor da carta entre 1 e 12
limitarValor :: Int -> Int
limitarValor valor
    | valor > 12 = 12
    | valor < 1  = 1
    | otherwise  = valor