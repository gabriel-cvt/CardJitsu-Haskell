module Services.Batalha where
    import Types.Carta
    import Types.Elemento
    import Types.Player

    -- Funções de cartas em Batalha!
    -- OBS: Acesso de valores feitos por pattern matching

    -- Básico de Combate

    combatePorElemento:: Carta -> Carta -> String
    combatePorElemento (Carta elJogador vJogador _ _) (Carta elBot vBot _ _)
        | prioridadeElemento elJogador elBot = "Vitória do jogador!"
        | prioridadeElemento elBot elJogador = "Jogador perdeu!"
        | otherwise = combatePorValor vJogador vBot

    combatePorValor:: Int -> Int -> String
    combatePorValor valorJogador valorBot
        | valorJogador > valorBot = "Vitória do jogador"
        | valorBot > valorJogador = "Jogador perdeu"
        | otherwise = "empate"

    -- Aplicar poder na carta

    aplicarMaisDois:: Carta -> Carta
    aplicarMaisDois (Carta elemento valor poder nome) = Carta elemento (valor + 2) poder nome

    aplicarMenosDois:: Carta -> Carta
    aplicarMenosDois (Carta elemento valor poder nome) = Carta elemento (valor - 2) poder nome

