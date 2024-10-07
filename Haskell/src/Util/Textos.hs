module Util.Textos (
    textoFaseInicial,
    textoFaseAzul,
    textoFaseRoxa,
    textoFaseMarrom,
    textoFaseFinal,
    textoZerou
) where
import Types.Player


textoFaseInicial :: IO()
textoFaseInicial = do 
    putStrLn "O ruivo é a primeira pessoa que aparece no Dojo"
    putStrLn "Ruivo: Você é novo por aqui, não é?"
    putStrLn "Ruivo: Vamos uma duelo para você entender como é o CardJitsu\n"

textoFaseAzul :: IO()
textoFaseAzul = do
    putStrLn "A bruxa da Neve se aproxima de forma sorrateira após você ganhar do Ruivo"
    putStrLn "Bruxa da Neve: O ruivo é mesmo um bobalhão, mas de minhas garras gélidas você não escapará!"
    putStrLn "Não restam opções para subir de faixa no dojo a não ser derrotando ela!\n"

textoFaseRoxa :: Player -> IO ()
textoFaseRoxa jogador = do
    putStrLn "Cavaleiro do mar: para derrotar a bruxa... requer um poder imenso, já foi perguntando como você fez esse feito"
    putStrLn $ "Como você conseguiu fazer isso, " ++ getNome jogador ++ "?"
    putStrLn "Você respondeu que somente batalhando para saber disso"
    putStrLn $ "Cavaleiro do mar: Então é assim, " ++ getNome jogador ++ "? É HORA DO DUELO!\n"

textoFaseMarrom :: Player -> IO ()
textoFaseMarrom jogador = do
    putStrLn "Após a derrota do Cavaleiro do Mar, o seu melhor amigo Punhos de Fogo apareceu correndo!"
    putStrLn "Ele está devastado! Qualquer coisa ao redor dele está em chamas!!"
    putStrLn "Punhos de Fogo: Não me importa quem é você ou o que você quer, minhas chamas irão te exterminar"
    putStrLn $ getNome jogador ++ ", como você ganhará dele??\n"

textoFaseFinal :: Player -> IO ()
textoFaseFinal jogador = do
    putStrLn "Olhos de Falcão, após ver todos os seus aprendizes derrotados, chegou em você para conversar"
    putStrLn $ "Olhos de Falcão: " ++ getNome jogador ++ ", escute o que tenho a dizer..."
    putStrLn "O verdadeiro guerreiro luta não porque odeia o que está à sua frente, mas porque ama o que está atrás dele."
    putStrLn "Chega de palavras agora, chegou a hora..."
    putStrLn "É HORA DO DUELO!\n"

textoZerou :: Player -> IO ()
textoZerou jogador = do
    putStrLn "Após derrotar o grande Sensei, você conseguiu alcançar algo que todos desejam..."
    putStrLn "Você encontrou finalmente o One pi..."
    putStrLn "A GRANDE MÁSCARA NINJA!"
    putStrLn $ "Após você conquistar esse feito, só restam duas opções, " ++ getNome jogador ++ "..."