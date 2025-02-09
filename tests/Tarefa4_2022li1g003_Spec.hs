module Tarefa4_2022li1g003_Spec where

import LI12223
import Tarefa4_2022li1g003
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ TestList [
    TestLabel "Testes jogoTerminou" testesJogoTerminou
    ]--test ["Teste 1" ~: 1 ~=? 1]

{- |
 A função @testesJogoTerminou@ testa a função @jogoTerminou@ utilizando diferentes conjuntos de dados, válidos no contexto da função.
-}
testesJogoTerminou = test [
    -- Testes envolvendo Rios, Estradas e Relvas
        -- Testes envolvendo largura 1
            "Teste 1 |" ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 1 [(Relva,[Nenhum])])),
            "Teste 2 |" ~: True ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 1 [(Rio 3,[Nenhum])])),            
            "Teste 3 |" ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 1 [(Estrada (-4),[Nenhum])])),
            "Teste 4 |" ~: True ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 1 [(Relva,[Nenhum])])),
            "Teste 5 |" ~: True ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 1 [(Estrada 1,[Nenhum])])),
            "Teste 6 |" ~: True ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 1 [(Estrada 0,[Nenhum]),(Rio (-2),[Nenhum])])),
            "Teste 7 |" ~: False ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 1 [(Estrada 1,[Nenhum]),(Relva,[Nenhum])])),
        
        -- Testes envolvendo largura 2
            "Teste 8 |" ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 9,[Nenhum,Nenhum])])),
            "Teste 9 |" ~: False ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada 2,[Carro,Nenhum])])),
            "Teste 10 |" ~: True ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada (-6),[Nenhum,Carro])])),
            "Teste 11 |" ~: False ~=? jogoTerminou (Jogo (Jogador (1,1)) (Mapa 2 [(Estrada 2,[Carro,Nenhum]),(Rio 4,[Nenhum,Tronco])])),
            "Teste 12 |" ~: True ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Arvore,Nenhum]),(Rio (-3),[Nenhum,Nenhum])])),
            "Teste 13 |" ~: True ~=? jogoTerminou (Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Arvore,Nenhum]),(Rio (-3),[Tronco,Nenhum])])),
            "Teste 14 |" ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum])])),
            "Teste 15 |" ~: False ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Arvore,Nenhum])]))                               
 ]