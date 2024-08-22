module Types.Carta (
    Poder (..),
    Carta (..)
) where

import Types.Elemento

data Poder = Bloqueia | MaisDois | MenosDois | Inverte | Null deriving (Eq, Show, Read)  

data Carta = Carta Elemento Int Poder String deriving (Eq, Show, Read)

