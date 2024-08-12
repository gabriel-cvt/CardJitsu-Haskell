module Types.Player where
    import Types.Carta
    import Types.Faixa

    data Player = Player String [Carta] Faixa Bool deriving (Show)



    -- método provisório
    getNome:: Player -> String
    getNome (Player nomePlayer _ _ _ ) = nomePlayer

    -- método provisório
    getFaixa:: Player -> Faixa
    getFaixa (Player _ _ faixaPlayer _ )= faixaPlayer
    
    -- Pega sucessor da faixa por enum
    upFaixa:: Player -> Player 
    upFaixa (Player nomePlayer cartas faixaPlayer mascara) = Player nomePlayer cartas (succ faixaPlayer) mascara

    -- não completo
    toString:: Player -> String
    toString (Player nomePlayer _ faixaPlayer mascaraNinja) = "Ninja: " ++ nomePlayer ++ "\nFaixa Atual: " ++ show faixaPlayer ++ "\nFaixas Adquiridas: "