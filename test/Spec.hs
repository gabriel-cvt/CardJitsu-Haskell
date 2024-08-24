-- test/Spec.hs

import Test.Tasty
import Test.Tasty.HUnit
import Types.Baralho (baralho, embaralhar)
import System.Random (newStdGen, randomRs)

main :: IO ()
main = defaultMain tests


-- Testes de Baralho :

tests :: TestTree
tests = testGroup "Tests" 
    [ 
        -- TESTES DE BARALHO
        testCase "Test baralho length" testBaralhoLength,
        testCase "Test embaralhar result length" testEmbaralharLength,
        testCase "Test if baralho is shuffled" testBaralhoEmbaralhado
    ]

-- Teste para verificar se o baralho tem 36 cartas
testBaralhoLength :: Assertion
testBaralhoLength = do
    let b = baralho
    length b @?= 36

-- Teste para verificar se o baralho embaralhado tem o mesmo comprimento
testEmbaralharLength :: Assertion
testEmbaralharLength = do
    let b = baralho
    bEmbaralhado <- embaralhar b
    length b @?= length bEmbaralhado

-- Teste para verificar se o baralho foi realmente embaralhado
testBaralhoEmbaralhado :: IO ()
testBaralhoEmbaralhado = do
    let originalBaralho = baralho
    embaralhado1 <- embaralhar originalBaralho
    embaralhado2 <- embaralhar originalBaralho
    assertBool "O baralho deveria estar embaralhado" (embaralhado1 /= originalBaralho)
    assertBool "O baralho deveria estar embaralhado" (embaralhado2 /= originalBaralho)
    assertBool "O baralho deveria estar embaralhado" (embaralhado1 /= embaralhado2)
    
