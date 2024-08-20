module Services.Batalha where
    import Types.Carta
    import Types.Elemento
    import Types.Player

    -- Funções de cartas em Batalha!
    -- OBS: Acesso de valores feitos por pattern matching

    -- Básico de Combate

    combatePorElemento:: Carta -> Carta -> String
    combatePorElemento (Carta eleJogador vJogador _ _) (Carta eleBot vBot _ _)
        | prioridadeElemento eleJogador eleBot = "W"
        | prioridadeElemento eleBot eleJogador = "L"
        | otherwise = combatePorValor vJogador vBot

    combatePorValor:: Int -> Int -> String
    combatePorValor valorJogador valorBot
        | valorJogador > valorBot = "W"
        | valorBot > valorJogador = "L"
        | otherwise = "D"

    -- Aplicar poder na carta

    aplicarMaisDois:: Carta -> Carta
    aplicarMaisDois (Carta elemento valor poder nome) = Carta elemento (valor + 2) poder nome

    aplicarMenosDois:: Carta -> Carta
    aplicarMenosDois (Carta elemento valor poder nome) = Carta elemento (valor - 2) poder nome

