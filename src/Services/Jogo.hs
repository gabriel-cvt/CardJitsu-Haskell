module Services.Jogo (
    iniciarJogo
) where

import Types.Baralho
import Types.Deque
import Types.Carta
import Services.Batalha

-- Tipo para representar o estado do jogo
data EstadoJogo = EstadoJogo {
    baralho :: Baralho,
    dequePlayer :: Deque,
    dequeBot :: Deque,
    historico :: [Carta]
} deriving (Show)

-- Função para iniciar o jogo
iniciarJogo :: IO ()
iniciarJogo = do
    -- Passo 1: Gerar Baralho
    let baralhoInicialJogador = criarBaralho
    
    -- Passo 2: Embaralhar
    baralhoEmbaralhado <- embaralhar baralhoInicialJogador
    
    -- Passo 3: Dar 5 cartas ao Player e Bot
    let (dequePlayer, baralhoRestante1) = criarDeque baralhoEmbaralhado
    let (dequeBot, baralhoRestante2) = criarDeque baralhoRestante1
    
    -- Inicializar estado do jogo
    let estadoInicial = EstadoJogo baralhoRestante2 dequePlayer dequeBot []
    
    -- Iniciar loop do jogo
    loopJogo estadoInicial

-- Função principal de loop do jogo
loopJogo :: EstadoJogo -> IO ()
loopJogo estado@(EstadoJogo baralho dequePlayer dequeBot historico) = do
    -- Passo 1: Verificar e aplicar poderes no topo do histórico
    let poderesTop = verificarPoderesTop historico
    aplicarPoderes poderesTop
    
    -- Passo 2: Player joga uma carta
    putStrLn "Escolha uma carta para jogar:"
    print dequePlayer
    escolha <- read <$> getLine
    let (cartaJogadaPlayer, novoDequePlayer) = jogarCarta escolha dequePlayer
    
    case cartaJogadaPlayer of
        Nothing -> do
            putStrLn "Escolha inválida. Tente novamente."
            loopJogo estado
        Just carta -> do
            -- Passo 3: Atualiza deque do player
            let novoHistorico = carta : historico
            
            -- Passo 4: Player ganha uma carta do baralho
            let (novoDequePlayer2, baralhoRestante3) = completarDeque novoDequePlayer baralho
            
            -- Passo 5: Bot joga uma carta (implementado com lógica básica)
            cartaBot <- escolherCartaBot dequeBot
            
            -- Passo 6: Atualiza deque do Bot
            let (novoDequeBot, baralhoRestante4) = completarDeque dequeBot baralhoRestante3
            
            -- Passo 7: Combate das duas cartas
            let vencedor = combate carta cartaBot
            
            -- Atualizar estado do jogo e continuar o loop
            let novoEstado = EstadoJogo baralhoRestante4 novoDequePlayer2 novoDequeBot (cartaBot : novoHistorico)
            putStrLn $ "Player jogou: " ++ show carta
            putStrLn $ "Bot jogou: " ++ show cartaBot
            putStrLn $ "Vencedor: " ++ show vencedor
            
            loopJogo novoEstado

verificarPoderesTop :: [Carta] -> [Poder]
verificarPoderesTop [] = []
verificarPoderesTop (carta:resto)
    | podeAplicarPoder carta = [poder carta]
    | otherwise = verificarPoderesTop resto

aplicarPoderes :: [Poder] -> IO ()
aplicarPoderes poderes = do
    let aplicadosPlayer = map aplicarPoderNoDeque poderes dequePlayer
    let aplicadosBot = map aplicarPoderNoDeque poderes dequeBot
    return ()

aplicarPoderNoDeque :: Poder -> Deque -> Deque
aplicarPoderNoDeque poder (Deque cartas) = Deque (map (aplicarPoder poder) cartas)

escolherCartaBot :: Deque -> IO Carta
escolherCartaBot (Deque cartas) = do
    let indice = length cartas `div` 2
    return $ cartas !! indice