module Tarefa3_2022li1g003_Spec where

import LI12223
import Tarefa3_2022li1g003
import Test.HUnit
import Util

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ TestList [
    TestLabel "Testes animaJogo" testesAnimaJogo
    ]

{- |
 A função @testesAnimaJogo@ testa a função @animaJogo@ utilizando diferentes conjuntos de dados, válidos no contexto da função.
-}
testesAnimaJogo = test [
    -- Testes envolvendo exclusivamente Relva
        -- Testes envolvendo largura 2
            "Teste 1 |" 
                ~: Jogo (Jogador (0,1)) (Mapa 2 [(Relva, [Nenhum, Arvore]),(Relva, [Nenhum, Nenhum])])
                ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 2 [(Relva, [Nenhum,Arvore]),(Relva, [Nenhum, Nenhum])])) (Move Esquerda),
            "Teste 2 |"
                ~: Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum, Arvore]),(Relva,[Nenhum, Nenhum])])
                ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 2 [(Relva, [Nenhum,Arvore]),(Relva, [Nenhum, Nenhum])])) (Move Direita),
            "Teste 3 |"
                ~: Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum, Arvore]),(Relva,[Nenhum, Arvore])])
                ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 2 [(Relva, [Nenhum,Arvore]),(Relva, [Nenhum, Arvore])])) (Move Cima),
            "Teste 4 |"
                ~: Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum, Arvore]),(Relva,[Nenhum, Arvore])])
                ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 2 [(Relva, [Nenhum,Arvore]),(Relva, [Nenhum, Arvore])])) (Move Baixo),
            "Teste 5 |"
                ~: Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum, Arvore]),(Relva,[Nenhum, Arvore])])
                ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 2 [(Relva, [Nenhum,Arvore]),(Relva, [Nenhum, Arvore])])) (Move Esquerda),
            "Teste 6 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum, Nenhum]),(Relva,[Nenhum, Arvore])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva, [Nenhum,Nenhum]),(Relva, [Nenhum, Arvore])])) (Move Direita),
            "Teste 7 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum, Nenhum]),(Relva,[Nenhum, Arvore])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva, [Nenhum,Nenhum]),(Relva, [Nenhum, Arvore])])) (Move Cima),
            "Teste 8 |"
                ~: Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum, Arvore]),(Relva,[Nenhum, Nenhum])])
                ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 2 [(Relva, [Nenhum,Arvore]),(Relva, [Nenhum, Nenhum])])) (Move Baixo),
            "Teste 9 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum, Nenhum]),(Relva,[Nenhum, Arvore])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva, [Nenhum,Nenhum]),(Relva, [Nenhum, Arvore])])) (Parado),
            
-- Faltam fazer testes quando o jogador está em nas mesmas coordenadas que uma arvore
-- a mesma coisa para um carro a esquerda, em cima e em baixo

    -- Testes envolvendo exclusivamente Rios
        -- Testes envolvendo largura 2
            "Teste 10 |"
                ~: Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 2,[Nenhum,Tronco])])
                ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 2,[Nenhum,Tronco])])) (Move Baixo),
            "Teste 11 |"
                ~: Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 0,[Nenhum,Tronco])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 0,[Nenhum,Tronco])])) (Move Baixo),
            "Teste 12 |"
                ~: Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco])])) (Move Baixo),
            "Teste 13 |"
                ~: Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 2,[Tronco,Nenhum])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 2,[Nenhum,Tronco])])) (Move Baixo),
            "Teste 14 |"
                ~: Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 2,[Nenhum,Tronco])])
                ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 2,[Nenhum,Tronco])])) (Move Baixo),
            "Teste 15 |"
                ~: Jogo (Jogador (0,0)) (Mapa 2 [(Rio 0,[Nenhum,Tronco])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Rio 0,[Nenhum,Tronco])])) (Move Esquerda),
            "Teste 16 |"
                ~: Jogo (Jogador (0,0)) (Mapa 3 [(Rio 0,[Tronco,Tronco,Nenhum])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 0,[Tronco,Tronco,Nenhum])])) (Move Esquerda),
            "Teste 17 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Rio 0,[Tronco,Nenhum])])
                ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 2 [(Rio 0,[Tronco,Nenhum])])) (Move Direita),
            "Teste 18 |"
                ~: Jogo (Jogador (2,0)) (Mapa 3 [(Rio 0,[Nenhum,Tronco,Tronco])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 0,[Nenhum,Tronco,Tronco])])) (Move Direita),
            "Teste 19 |"
                ~: Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 2,[Nenhum,Tronco])])
                ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 2,[Nenhum,Tronco])])) (Move Cima),
            "Teste 20 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 0,[Nenhum,Tronco])])
                ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 0,[Nenhum,Tronco])])) (Move Cima),

    -- Testes envolvendo exclusivamente Estradas
        -- Testes envolvendo largura 2
            "Teste 21 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Carro])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Carro])])) (Move Baixo),
            "Teste 22 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 1,[Nenhum,Carro])])) (Move Baixo),
            "Teste 23 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro])])) (Move Baixo),
            "Teste 24 |"
                ~: Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 0,[Nenhum,Carro])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada 0,[Nenhum,Carro])])) (Move Esquerda),
            "Teste 25 |"
                ~: Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 0,[Carro,Carro,Nenhum])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 0,[Carro,Carro,Nenhum])])) (Move Esquerda),
            "Teste 26 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Estrada 0,[Carro,Nenhum])])
                ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 0,[Carro,Nenhum])])) (Move Direita),
            "Teste 27 |"
                ~: Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 0,[Nenhum,Carro,Carro])])
                ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 0,[Nenhum,Carro,Carro])])) (Move Direita),
            "Teste 28 |"
                ~: Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum])])
                ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro])])) (Move Cima),
            "Teste 29 |"
                ~: Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Carro])])
                ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Carro])])) (Move Cima)
    ]