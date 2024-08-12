module Types.Carta where
    import Types.Elemento
    
    data Poder = Bloqueia | MaisDois | MenosDois | Inverte | Null deriving (Eq, Show)  

    data Carta = Carta Elemento Int Poder String deriving (Eq, Show)

    