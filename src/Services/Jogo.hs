module Services.Jogo (
    iniciarJogo
) where

import Types.Baralho
import Types.Deque
import Types.Carta
import Services.Batalha
import Types.Elemento (Elemento(..))
import System.Console.ANSI (clearScreen)

data EstadoJogo = EstadoJogo {
    baralhoPlayer :: Baralho,
    elementosPlayer :: ([Elemento], [Elemento], [Elemento]),
    baralhoInimigo :: Baralho,
    elementosInimigo :: ([Elemento], [Elemento], [Elemento]),
    dequePlayer :: Deque,
    dequeInimigo :: Deque,
    pilhaPoderPlayer :: [Poder],
    pilhaPoderInimigo :: [Poder]
} deriving (Show)

-- Inicia o jogo e executa o loop principal do jogo
iniciarJogo :: IO Bool
iniciarJogo = do
    -- Passo 1: Gerar e embaralhar Baralhos
    baralhoJogador <- embaralhar novoBaralho
    baralhoInimigo <- embaralhar novoBaralho

    -- Passo 2: Distribuir cartas iniciais
    let (dequeJogador, baralhoRestanteJogador) = novoDeque baralhoJogador
        (dequeInimigo, baralhoRestanteInimigo) = novoDeque baralhoInimigo

    -- Inicializar estado do jogo
    let estadoInicial = EstadoJogo baralhoRestanteJogador ([], [], []) baralhoRestanteInimigo ([], [], []) dequeJogador dequeInimigo [] []

    -- Iniciar loop do jogo
    loopJogo estadoInicial

-- Loop principal do jogo
loopJogo :: EstadoJogo -> IO Bool
loopJogo estado = do
    clearScreen  -- Limpa a tela a cada iteração do loop

    -- Exibir elementos acumulados pelo jogador
    putStrLn "Seus elementos acumulados:"
    printElementos (elementosPlayer estado)
    putStrLn ""

    -- Exibir elementos acumulados pelo inimigo
    putStrLn "Elementos acumulados pelo inimigo:"
    printElementosInimigo (elementosInimigo estado)
    putStrLn ""

    -- Exibir poderes ativos
    putStrLn "Poder Ativo:"
    printPoderesAtivos (pilhaPoderPlayer estado) (pilhaPoderInimigo estado)
    putStrLn ""

    -- Determinar elemento proibido baseado no poder ativo do jogador e do inimigo
    let elementoProibidoPlayer = case pilhaPoderPlayer estado of
            (Bloquear elem : _) -> Just elem
            _ -> Nothing
        elementoProibidoInimigo = case pilhaPoderInimigo estado of
            (Bloquear elem : _) -> Just elem
            _ -> Nothing

    -- Jogador faz sua jogada
    putStrLn "Escolha uma carta para jogar [1-5]:\n"
    print (dequePlayer estado)
    
    escolha <- read <$> getLine
    let escolhaAjustada = escolha - 1
    let (cartaJogadaPlayer, novoDequePlayer) = jogarCarta escolhaAjustada (dequePlayer estado)
    
    case cartaJogadaPlayer of
        Nothing -> do
            putStrLn "Escolha inválida. Tente novamente."
            pauseScreen
            loopJogo estado
        Just cartaPlayer -> do
            -- Verifica se a carta jogada é do elemento proibido
            case elementoProibidoPlayer of
                Just elem | cartaElemento cartaPlayer == elem -> do
                    putStrLn $ "Não é permitido jogar a carta do elemento " ++ show elem ++ " por causa do poder bloqueado pelo jogador."
                    pauseScreen
                    loopJogo estado
                _ -> do
                    -- Atualizar estado do jogo após a jogada do jogador
                    let novaPilhaPlayer = atualizarPilhaPoder cartaPlayer (pilhaPoderPlayer estado)
                        (novoDequePlayer2, baralhoPlayerRestante) = completarDeque novoDequePlayer (baralhoPlayer estado)

                    -- Inimigo faz sua jogada usando inimigoPegarCarta
                    (cartaJogadaInimigo, baralhoInimigoRestante) <- inimigoPegarCarta (baralhoInimigo estado) elementoProibidoInimigo
                    let (novoDequeInimigo, baralhoInimigoRestanteFinal) = completarDeque (dequeInimigo estado) baralhoInimigoRestante

                    case cartaJogadaInimigo of
                        Nothing -> do
                            putStrLn "O inimigo não tem cartas para jogar. O jogo acabou!"
                            pauseScreen
                            return True
                        Just cartaBot -> do
                            -- Mostrar carta jogada pelo inimigo
                            putStrLn "O inimigo jogou:"
                            print cartaBot
                            pauseScreen

                            -- Resolver combate e atualizar estado do jogo
                            let vencedor = combate cartaPlayer cartaBot
                                novoElementosPlayer = if vencedor == JOGADOR then atualizarElementos (elementosPlayer estado) cartaPlayer else elementosPlayer estado
                                novoElementosInimigo = if vencedor == ADVERSARIO then atualizarElementos (elementosInimigo estado) cartaBot else elementosInimigo estado
                                novaPilhaPlayer = atualizarPilhaPoder cartaPlayer (pilhaPoderPlayer estado)
                                novaPilhaInimigo = atualizarPilhaPoder cartaBot (pilhaPoderInimigo estado)
                            
                            -- Verificar se há um vencedor
                            if verificaVencedor novoElementosPlayer
                            then do
                                putStrLn "Você venceu o jogo!"
                                pauseScreen
                                return True
                            else if verificaVencedor novoElementosInimigo
                                then do
                                    putStrLn "O inimigo venceu o jogo!"
                                    pauseScreen
                                    return False
                                else do
                                    -- Continuar o loop com o estado atualizado
                                    let novoEstado = EstadoJogo baralhoPlayerRestante novoElementosPlayer baralhoInimigoRestanteFinal novoElementosInimigo novoDequePlayer2 novoDequeInimigo novaPilhaPlayer novaPilhaInimigo
                                    loopJogo novoEstado

-- Função para imprimir os poderes ativos
printPoderesAtivos :: [Poder] -> [Poder] -> IO ()
printPoderesAtivos poderesPlayer poderesInimigo = do
    let poderPlayer = if null poderesPlayer then "Nenhum" else show (head poderesPlayer)
    let poderInimigo = if null poderesInimigo then "Nenhum" else show (head poderesInimigo)
    putStrLn $ "| Seu Poder : " ++ poderPlayer
    putStrLn $ "| Poder Inimigo: " ++ poderInimigo

-- Função auxiliar para pausar e esperar input do usuário
pauseScreen :: IO ()
pauseScreen = do
    putStrLn "Pressione Enter para continuar..."
    _ <- getLine
    return ()

-- Atualiza os elementos de acordo com o vencedor da rodada
atualizarElementos :: ([Elemento], [Elemento], [Elemento]) -> Carta -> ([Elemento], [Elemento], [Elemento])
atualizarElementos (fogo, agua, neve) (Carta elemento _ _) =
    case elemento of
        Fogo -> (Fogo : fogo, agua, neve)
        Agua -> (fogo, Agua : agua, neve)
        Neve -> (fogo, agua, Neve : neve)

-- Função para imprimir os elementos acumulados
printElementos :: ([Elemento], [Elemento], [Elemento]) -> IO ()
printElementos (fogo, agua, neve) = do
    putStrLn $ "| Fogo: " ++ show (length fogo)
    putStrLn $ "| Água: " ++ show (length agua)
    putStrLn $ "| Neve: " ++ show (length neve)

printElementosInimigo :: ([Elemento], [Elemento], [Elemento]) -> IO ()
printElementosInimigo (fogo, agua, neve) = do
    putStrLn $ "| Fogo: " ++ show (length fogo)
    putStrLn $ "| Água: " ++ show (length agua)
    putStrLn $ "| Neve: " ++ show (length neve)

-- Verifica se há um vencedor baseado nos elementos acumulados
verificaVencedor :: ([Elemento], [Elemento], [Elemento]) -> Bool
verificaVencedor (fogo, agua, neve) =
    let counts = map length [fogo, agua, neve]
    in any (>= 3) counts || all (> 0) counts

-- Função auxiliar para verificar o elemento da carta
cartaElemento :: Carta -> Elemento
cartaElemento (Carta elemento _ _) = elemento