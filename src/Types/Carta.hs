module Types.Carta (
    Carta (..),
    Poder (..)
) where

import Types.Elemento

data Poder = Bloqueia | MaisDois | MenosDois | Inverte | Null deriving (Eq, Read)  

data Carta = Carta Elemento Int Poder deriving (Eq, Read)

instance Show Poder where
    show Bloqueia   = "[X]"
    show MaisDois   = "[+2]"
    show MenosDois  = "[-2]"
    show Inverte    = "[⮌]"
    show Null       = ""

instance Show Carta where
    show (Carta elemento valor poder) =
        case poder of
            Null -> "( " ++ show elemento ++ " : " ++ show valor ++ " )"
            _    -> "( " ++ show elemento ++ " : " ++ show valor ++ " -> " ++ show poder ++ " )"