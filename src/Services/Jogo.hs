module Services.Jogo (
    iniciarJogo
) where

import Types.Baralho
import Types.Deque
import Types.Carta
import Services.Batalha
import Types.Elemento (Elemento (..))
import System.Console.ANSI (clearScreen)
import Types.Player (getNome, Player, Faixa (..), getFaixa)
import Util.Lib (carregaJogador, pressionarTecla, pausarJogo)

data EstadoJogo = EstadoJogo {
    baralhoPlayer :: Baralho,
    elementosPlayer :: ([Elemento], [Elemento], [Elemento]),
    baralhoInimigo :: Baralho,
    elementosInimigo :: ([Elemento], [Elemento], [Elemento]),
    dequePlayer :: Deque,
    dequeInimigo :: Deque,
    pilhaPoder :: [Poder]
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
    let estadoInicial = EstadoJogo baralhoRestanteJogador ([], [], []) baralhoRestanteInimigo ([], [], []) dequeJogador dequeInimigo []

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

    -- Exibir poderes ativos
    putStrLn "Poder Ativo:"
    printPoderesAtivos (pilhaPoder estado)
    putStrLn ""

    -- Jogador faz sua jogada
    putStrLn "Escolha uma carta para jogar [1-5]:"
    putStrLn "Caso queira pausar o jogo, aperte 6\n"
    print (dequePlayer estado)
    putStrLn "[6] - Pausar Jogo"
    escolha <- read <$> getLine

    -- Ajustar o índice para a base 0
    let escolhaAjustada = escolha - 1
    if escolhaAjustada == 5 then do
        clearScreen >> pausarJogo >> loopJogo estado
    else do
        let (cartaJogadaPlayer, novoDequePlayer) = jogarCarta escolhaAjustada (dequePlayer estado)
        
        case cartaJogadaPlayer of
            Nothing -> do
                putStrLn "Escolha inválida. Tente novamente."
                pressionarTecla
                loopJogo estado
            Just cartaPlayer -> do
                -- Atualizar estado do jogo após a jogada do jogador
                let novaPilha = atualizarPilhaPoder cartaPlayer (pilhaPoder estado)
                    (novoDequePlayer2, baralhoPlayerRestante) = completarDeque novoDequePlayer (baralhoPlayer estado)

                -- Inimigo faz sua jogada
                let (cartaJogadaInimigo, baralhoInimigoRestante) = pegarCarta (baralhoInimigo estado)
                    (novoDequeInimigo, baralhoInimigoRestanteFinal) = completarDeque (dequeInimigo estado) baralhoInimigoRestante

                case cartaJogadaInimigo of
                    Nothing -> do
                        putStrLn "O inimigo não tem cartas para jogar. O jogo acabou!"
                        pressionarTecla
                        return True
                    Just cartaBot -> do
                        -- Mostrar carta jogada pelo inimigo
                        putStrLn "O inimigo jogou:"
                        print cartaBot
                        pressionarTecla

                        -- Resolver combate e atualizar estado do jogo
                        let vencedor = combate cartaPlayer cartaBot
                            novoElementosPlayer = if vencedor == JOGADOR then atualizarElementos (elementosPlayer estado) cartaPlayer else elementosPlayer estado
                            novoElementosInimigo = if vencedor == ADVERSARIO then atualizarElementos (elementosInimigo estado) cartaBot else elementosInimigo estado
                        
                        -- Verificar se há um vencedor
                        if verificaVencedor novoElementosPlayer
                        then do
                            clearScreen
                            putStrLn "Você venceu o jogo!"
                            pressionarTecla
                            return True
                        else if verificaVencedor novoElementosInimigo
                            then do
                                putStrLn "O inimigo venceu o jogo!"
                                pressionarTecla
                                return False
                            else do
                                -- Continuar o loop com o estado atualizado
                                let novoEstado = EstadoJogo baralhoPlayerRestante novoElementosPlayer baralhoInimigoRestanteFinal novoElementosInimigo novoDequePlayer2 novoDequeInimigo novaPilha
                                loopJogo novoEstado


-- Função para imprimir os poderes ativos
printPoderesAtivos :: [Poder] -> IO ()
printPoderesAtivos poderes = do
    player <- carregaJogador
    let poderPlayer = if null poderes then "Nenhum" else show (head poderes)
    let poderInimigo = if length poderes < 2 then "Nenhum" else show (poderes !! 1)
    putStrLn $ "| " ++ getNome player ++ ": " ++ poderPlayer
    putStrLn $ "| " ++ printNomeInimigo player ++ ": " ++ poderInimigo

-- Função auxiliar de bônus pra printar nome do inimigo :)
printNomeInimigo :: Player -> String
printNomeInimigo jogador = do
    case getFaixa jogador of
        Branca -> "O Ruivo"
        Azul -> "Bruxa da Neve"
        Roxa -> "Cavaleiro do Mar"
        Marrom -> "Punhos de Fogo"
        Preta -> "Olhos de falcão"
        _ -> "Inimigo"
        
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

-- Verifica se há um vencedor baseado nos elementos acumulados
verificaVencedor :: ([Elemento], [Elemento], [Elemento]) -> Bool
verificaVencedor (fogo, agua, neve) =
    let counts = map length [fogo, agua, neve]
    in any (>= 3) counts || all (> 0) counts