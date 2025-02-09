module Tarefa5_2022li1g003_Spec where

import LI12223
import Tarefa5_2022li1g003
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 5" $ TestList [
    TestLabel "Testes deslizaJogo" testesDeslizaJogo
    ]--test ["Teste 1" ~: 1 ~=? 1]

{- |
 A função @testesDeslizaJogo@ testa a função @deslizaJogo@ utilizando diferentes conjuntos de dados, válidos no contexto da função.
-}
testesDeslizaJogo = test [
    -- Testes envolvendo Rios, Estradas e Relvas
        -- Testes envolvendo largura 1
            "Teste 1 |" 
                ~: Jogo (Jogador (0,2)) (Mapa 1 [(Estrada 1,[Nenhum])])
                ~=? deslizaJogo 7 (Jogo (Jogador (0,1)) (Mapa 1 [(Relva,[Nenhum])])),
            "Teste 2 |" 
                ~: Jogo (Jogador (0,1)) (Mapa 1 [(Estrada 1,[Nenhum])])
                ~=? deslizaJogo 1 (Jogo (Jogador (0,0)) (Mapa 1 [(Rio 3,[Nenhum])])),            
            "Teste 3 |" 
                ~: Jogo (Jogador (0,1)) (Mapa 1 [(Estrada 1,[Nenhum])])
                ~=? deslizaJogo 91 (Jogo (Jogador (0,0)) (Mapa 1 [(Estrada (-4),[Nenhum])])),
            "Teste 4 |" 
                ~: Jogo (Jogador (1,1)) (Mapa 1 [(Estrada 1,[Nenhum])])
                ~=? deslizaJogo 4 (Jogo (Jogador (1,0)) (Mapa 1 [(Relva,[Nenhum])])),
            "Teste 5 |" 
                ~: Jogo (Jogador (0,2)) (Mapa 1 [(Relva,[Nenhum])])
                ~=? deslizaJogo 95 (Jogo (Jogador (0,1)) (Mapa 1 [(Estrada 1,[Nenhum])])),
            "Teste 6 |" 
                ~: Jogo (Jogador (0,2)) (Mapa 1 [(Estrada 1,[Nenhum]),(Estrada 0,[Nenhum])])
                ~=? deslizaJogo 73 (Jogo (Jogador (0,1)) (Mapa 1 [(Estrada 0,[Nenhum]),(Rio (-2),[Nenhum])])),
            "Teste 7 |" 
                ~: Jogo (Jogador (0,2)) (Mapa 1 [(Relva,[Nenhum]),(Estrada 1,[Nenhum])])
                ~=? deslizaJogo 14 (Jogo (Jogador (0,1)) (Mapa 1 [(Estrada 1,[Nenhum]),(Relva,[Nenhum])])),
        
        -- Testes envolvendo largura 2
            "Teste 8 |" 
                ~: Jogo (Jogador (0,1)) (Mapa 2 [(Estrada 1,[Carro,Nenhum])])
                ~=? deslizaJogo 10 (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 9,[Nenhum,Nenhum])])),
            "Teste 9 |" 
                ~: Jogo (Jogador (1,1)) (Mapa 2 [(Rio 1,[Tronco,Nenhum])])
                ~=? deslizaJogo 3 (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada 2,[Carro,Nenhum])])),
            "Teste 10 |" 
                ~: Jogo (Jogador (1,1)) (Mapa 2 [(Rio 1,[Tronco,Nenhum])])
                ~=? deslizaJogo 12 (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada (-6),[Nenhum,Carro])])),
            "Teste 11 |" 
                ~: Jogo (Jogador (1,2)) (Mapa 2 [(Relva,[Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum])])
                ~=? deslizaJogo 71 (Jogo (Jogador (1,1)) (Mapa 2 [(Estrada 2,[Carro,Nenhum]),(Rio 4,[Nenhum,Tronco])])),
            "Teste 12 |" 
                ~: Jogo (Jogador (0,2)) (Mapa 2 [(Rio (-1),[Nenhum,Nenhum]),(Relva,[Arvore,Nenhum])]) 
                ~=? deslizaJogo (-12) (Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Arvore,Nenhum]),(Rio (-3),[Nenhum,Nenhum])])),
            "Teste 13 |" 
                ~: Jogo (Jogador (1,2)) (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Relva,[Arvore,Nenhum])])
                ~=? deslizaJogo 81 (Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Arvore,Nenhum]),(Rio (-3),[Tronco,Nenhum])])),
            "Teste 14 |" 
                ~: Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum,Arvore])])
                ~=? deslizaJogo 41 (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum])])),
            "Teste 15 |" 
                ~: Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum,Arvore])])
                ~=? deslizaJogo 62 (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Arvore,Nenhum])]))                              
 ]