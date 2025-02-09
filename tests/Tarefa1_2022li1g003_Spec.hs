module Tarefa1_2022li1g003_Spec where

import LI12223
import Tarefa1_2022li1g003
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ TestList [
    TestLabel "Testes mapaValido" testesMapaValido
    ]--test ["Teste 1" ~: 1 ~=? 1]

{- |
 A função @testesMapaValido@ testa a função @mapaValido@ utilizando diferentes conjuntos de dados, válidos no contexto da função.
-}
testesMapaValido = test [
    -- Testes envolvendo exclusivamente Rios
        -- Testes envolvendo largura 1
            "Teste 1 |" ~: True ~=? mapaValido (Mapa 1 [(Rio 2, [Nenhum])]),
            "Teste 2 |" ~: True ~=? mapaValido (Mapa 1 [(Rio 0, [Nenhum])]),
            "Teste 3 |" ~: False ~=? mapaValido (Mapa 1 [(Rio (-2), [Tronco])]),
            "Teste 4 |" ~: False ~=? mapaValido (Mapa 1 [(Rio 4, [Nenhum,Nenhum])]),
            "Teste 5 |" ~: False ~=? mapaValido (Mapa 1 [(Rio 8, [Carro])]),
            "Teste 6 |" ~: False ~=? mapaValido (Mapa 1 [(Rio (-5), [Arvore])]),
            "Teste 7 |" ~: True ~=? mapaValido (Mapa 1 [(Rio 3, [Nenhum]),(Rio (-7), [Nenhum])]),
            "Teste 8 |" ~: False ~=? mapaValido (Mapa 1 [(Rio 6, [Nenhum]),(Rio 2, [Nenhum])]),
            "Teste 9 |" ~: False ~=? mapaValido (Mapa 1 [(Rio 9, [Nenhum]),(Rio (-1), [Nenhum]),(Rio 8, [Nenhum]), (Rio (-3), [Nenhum]), (Rio 7, [Nenhum])]),
        -- Testes envolvendo largura 2
            "Teste 10 |" ~: True ~=? mapaValido (Mapa 2 [(Rio 5, [Nenhum,Nenhum])]),
            "Teste 11 |" ~: True ~=? mapaValido (Mapa 2 [(Rio 0, [Nenhum,Tronco])]),
            "Teste 12 |" ~: False ~=? mapaValido (Mapa 2 [(Rio 7, [Tronco,Tronco])]),
            "Teste 13 |" ~: False ~=? mapaValido (Mapa 2 [(Rio 2, [Nenhum, Carro])]),
            "Teste 14 |" ~: False ~=? mapaValido (Mapa 2 [(Rio (-1), [Nenhum, Arvore])]),
        -- Testes envolvendo largura 7
            "Teste 15 |" ~: True ~=? mapaValido (Mapa 7 [(Rio 3, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])]),
            "Teste 16 |" ~: False ~=? mapaValido (Mapa 7 [(Rio (-4), [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])]),
    -- Testes envolvendo exclusivamente Estradas
        -- Testes envolvendo largura 1
            "Teste 17 |" ~: True ~=? mapaValido (Mapa 1 [(Estrada 2, [Nenhum])]),
            "Teste 18 |" ~: True ~=? mapaValido (Mapa 1 [(Estrada 0, [Nenhum])]),
            "Teste 19 |" ~: False ~=? mapaValido (Mapa 1 [(Estrada (-3), [Nenhum,Nenhum])]),
            "Teste 20 |" ~: False ~=? mapaValido (Mapa 1 [(Estrada (-3), [Carro])]),
            "Teste 21 |" ~: False ~=? mapaValido (Mapa 1 [(Estrada 7, [Tronco])]),
            "Teste 22 |" ~: False ~=? mapaValido (Mapa 1 [(Estrada (-2), [Arvore])]),
            "Teste 23 |" ~: True ~=? mapaValido (Mapa 1 [(Estrada (-1), [Nenhum]),(Estrada (-4), [Nenhum])]),
            "Teste 24 |" ~: False ~=? mapaValido (Mapa 1 [(Estrada 3, [Nenhum]),(Estrada 8, [Nenhum]),(Estrada 2, [Nenhum]),(Estrada 4, [Nenhum]),(Estrada (-8), [Nenhum]),(Estrada 5, [Nenhum])]),
        -- Testes envolvendo largura 2
            "Teste 25 |" ~: True ~=? mapaValido (Mapa 2 [(Estrada 9, [Nenhum, Carro])]),
            "Teste 26 |" ~: False ~=? mapaValido (Mapa 2 [(Estrada (-8), [Nenhum,Tronco])]),
            "Teste 27 |" ~: False ~=? mapaValido (Mapa 2 [(Estrada 5, [Nenhum, Arvore])]),
            "Teste 28 |" ~: False ~=? mapaValido (Mapa 2 [(Estrada 1, [Carro,Carro])]),
        -- Testes envolvendo largura 5
            "Teste 29 |" ~: True ~=? mapaValido (Mapa 5 [(Estrada 2, [Nenhum,Carro,Carro,Carro,Nenhum])]),
            "Teste 30 |" ~: False ~=? mapaValido (Mapa 5 [(Estrada (-4), [Carro,Carro,Carro,Carro,Nenhum])]),
    -- Testes envolvendo exclusivamente Relvas
        -- Testes envolvendo largura 1
            "Teste 31 |" ~: True ~=? mapaValido (Mapa 1 [(Relva, [Nenhum])]),
            "Teste 32 |" ~: False ~=? mapaValido (Mapa 1 [(Relva, [Arvore])]),
            "Teste 33 |" ~: False ~=? mapaValido (Mapa 1 [(Relva, [Carro])]),
            "Teste 34 |" ~: False ~=? mapaValido (Mapa 1 [(Relva, [Tronco])]),
            "Teste 35 |" ~: False ~=? mapaValido (Mapa 1 [(Relva, [Arvore,Nenhum])]),
            "Teste 36 |" ~: False ~=? mapaValido (Mapa 1 [(Relva, [Nenhum]),(Relva, [Nenhum]),(Relva, [Nenhum]),(Relva, [Nenhum]),(Relva, [Nenhum]),(Relva, [Nenhum])]),
        -- Testes envolvendo largura 2
            "Teste 37 |" ~: True ~=? mapaValido (Mapa 2 [(Relva, [Nenhum,Arvore])]),
            "Teste 38 |" ~: False ~=? mapaValido (Mapa 2 [(Relva, [Nenhum,Carro])]),
            "Teste 39 |" ~: False ~=? mapaValido (Mapa 2 [(Relva, [Nenhum,Tronco])]),
            "Teste 40 |" ~: False ~=? mapaValido (Mapa 2 [(Relva, [Arvore,Arvore])]),
    -- Testes envolvendo Rios, Estradas e Relvas
        -- Testes envolvendo largura 1
            "Teste 41 |" ~: True ~=? mapaValido (Mapa 1 [(Relva, [Nenhum]),(Estrada 3, [Nenhum]),(Rio (-8), [Nenhum])]),
            "Teste 42 |" ~: False ~=? mapaValido (Mapa 1 [(Estrada (-2), [Carro]),(Rio 2, [Nenhum]),(Relva, [Nenhum])]),
        -- Testes envolvendo largura 2
            "Teste 43 |" ~: True ~=? mapaValido (Mapa 2 [(Rio 2, [Tronco,Nenhum]),(Rio (-1), [Nenhum,Tronco]),(Estrada 5, [Nenhum,Carro])]),
            "Teste 44 |" ~: False ~=? mapaValido (Mapa 2 [(Estrada 9, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio 6, [Tronco,Nenhum])]),
        -- Testes envolvendo largura 7
            "Teste 45 |" ~: True ~=? mapaValido (Mapa 7 [(Relva, [Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Arvore]),(Estrada 3, [Nenhum,Carro,Carro,Carro,Nenhum,Carro,Carro]),(Estrada 12, [Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Carro]),(Relva, [Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum]),(Rio (-1), [Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio 3, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])]),
            "Teste 46 |" ~: False ~=? mapaValido (Mapa 7 [(Rio 1, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum]),(Rio 8, [Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum])]),
            "Teste 47 |" ~: False ~=? mapaValido (Mapa 7[(Estrada (-2), [Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Carro]),(Estrada (-9), [Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro]),(Rio (-1), [Nenhum,Carro,Tronco,Tronco,Nenhum,Nenhum]),(Rio 9, [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])])
 ]