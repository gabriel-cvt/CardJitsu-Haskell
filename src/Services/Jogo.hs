module Services.Jogo (
    iniciarJogo
) where

import Types.Baralho
import Types.Deque
import Types.Carta
import Services.Batalha
import Types.Elemento (Elemento (..))

data EstadoJogo = EstadoJogo {
    baralhoPlayer :: Baralho,
    elementosPlayer :: ([Elemento], [Elemento], [Elemento]),
    baralhoInimigo :: Baralho,
    elementosInimigo :: ([Elemento], [Elemento], [Elemento]),
    dequePlayer :: Deque,
    dequeInimigo :: Deque,
    pilhaPoder :: [Poder]
} deriving (Show)

iniciarJogo :: IO Bool
iniciarJogo = do
    -- Passo 1: Gerar Baralho
    let baralho = novoBaralho

    -- Passo 2: Embaralhar Baralhos Separadamente
    baralhoJogador <- embaralhar baralho
    baralhoInimigo <- embaralhar baralho

    -- Passo 3: Dar 5 cartas ao Jogador
    let (dequeJogador, baralhoRestanteJogador) = novoDeque baralhoJogador

    -- Passo 4: Dar 5 cartas ao Inimigo
    let (dequeInimigo, baralhoRestanteInimigo) = novoDeque baralhoInimigo

    -- Inicializar estado do jogo
    let estadoInicial = EstadoJogo baralhoRestanteJogador ([], [], []) baralhoRestanteInimigo ([], [], []) dequeJogador dequeInimigo []

    -- Iniciar loop do jogo e retornar o resultado
    loopJogo estadoInicial


loopJogo :: EstadoJogo -> IO Bool
loopJogo estado = do
    -- Passo 1: Jogador joga uma carta
    putStrLn "Escolha uma carta para jogar (índice):"
    print (dequePlayer estado)
    escolha <- read <$> getLine
    let (cartaJogadaPlayer, novoDequePlayer) = jogarCarta escolha (dequePlayer estado)
    
    case cartaJogadaPlayer of
        Nothing -> do
            putStrLn "Escolha inválida. Tente novamente."
            loopJogo estado
        Just cartaPlayer -> do
            -- Passo 2: Atualiza pilha de Poder
            let novaPilha = atualizarPilhaPoder cartaPlayer (pilhaPoder estado)
            
            -- Passo 3: Player ganha uma carta do baralho
            let (novoDequePlayer2, baralhoPlayerRestante) = completarDeque novoDequePlayer (baralhoPlayer estado)
            
            -- Passo 4: Inimigo joga uma carta
            let (cartaJogadaInimigo, baralhoInimigoRestante) = pegarCarta (baralhoInimigo estado)
            let (novoDequeInimigo, baralhoInimigoRestanteFinal) = completarDeque (dequeInimigo estado) baralhoInimigoRestante
            
            case cartaJogadaInimigo of
                Nothing -> do
                    putStrLn "O inimigo não tem cartas para jogar. O jogo acabou!"
                    return True
                Just cartaBot -> do
                    -- Passo 5: Combate das duas cartas
                    let vencedor = combate cartaPlayer cartaBot
                    
                    -- Atualiza elementos
                    let novoElementosPlayer = atualizarElementos vencedor (elementosPlayer estado) cartaPlayer
                    let novoElementosInimigo = atualizarElementos (oponente vencedor) (elementosInimigo estado) cartaBot
                    
                    -- Verificar se há um vencedor
                    if verificaVencedor novoElementosPlayer
                    then do
                        putStrLn "Você venceu o jogo!"
                        return True
                    else if verificaVencedor novoElementosInimigo
                        then do
                            putStrLn "O inimigo venceu o jogo!"
                            return False
                        else do
                            -- Atualiza estado do jogo e continua o loop
                            let novoEstado = EstadoJogo baralhoPlayerRestante novoElementosPlayer baralhoInimigoRestanteFinal novoElementosInimigo novoDequePlayer2 novoDequeInimigo novaPilha
                            loopJogo novoEstado

atualizarElementos :: VencedorRodada -> ([Elemento], [Elemento], [Elemento]) -> Carta -> ([Elemento], [Elemento], [Elemento])
atualizarElementos vencedor (fogo, agua, neve) (Carta elemento _ _) =
    case vencedor of
        JOGADOR -> adicionarElemento elemento (fogo, agua, neve)
        ADVERSARIO -> adicionarElemento elemento (fogo, agua, neve)
        NENHUM -> (fogo, agua, neve)

adicionarElemento :: Elemento -> ([Elemento], [Elemento], [Elemento]) -> ([Elemento], [Elemento], [Elemento])
adicionarElemento Fogo (fogo, agua, neve) = (Fogo : fogo, agua, neve)
adicionarElemento Agua (fogo, agua, neve) = (fogo, Agua : agua, neve)
adicionarElemento Neve (fogo, agua, neve) = (fogo, agua, Neve : neve)

oponente :: VencedorRodada -> VencedorRodada
oponente JOGADOR = ADVERSARIO
oponente ADVERSARIO = JOGADOR
oponente NENHUM = NENHUM

verificaVencedor :: ([Elemento], [Elemento], [Elemento]) -> Bool
verificaVencedor (fogo, agua, neve) =
    let counts = map length [fogo, agua, neve]
    in any (>= 3) counts || all (> 0) counts

atualizarPilhaPoder :: Carta -> [Poder] -> [Poder]
atualizarPilhaPoder (Carta  _ _ Null) pilha = pilha
atualizarPilhaPoder (Carta  _ _ poder) pilha = poder : pilha