module Types.Elemento (
    Elemento(..),
    prioridadeElemento
) where

data Elemento = Fogo | Agua | Neve deriving (Eq, Read)

instance Show Elemento where
    show Fogo = "FOGO"
    show Agua = "ÁGUA"
    show Neve = "NEVE"

prioridadeElemento :: Elemento -> Elemento -> Bool
prioridadeElemento Fogo Neve = True
prioridadeElemento Neve Agua = True
prioridadeElemento Agua Fogo = True
prioridadeElemento _ _ = False
