{-# LANGUAGE ScopedTypeVariables #-}
module Tarefa2_2022li1g003_Spec where

import LI12223
import Tarefa1_2022li1g003
import Tarefa2_2022li1g003
import Test.HUnit
import Util (filterIndexed, getTimeSA, getPRand, getTerrenoConstr, anyIndexed, throws)
import Data.List
import Control.Exception (SomeException, catch)

import Data.Functor

{-
 Para facilitar a leitura e reduzir a extensão dos nomes das funções neste ficheiro, as seguintes nomenclaturas são utilizadas:

 ABREVIATURAS:
    - extendeMapa: EM
    - proximosTerrenosValidos: PTV
    - proximosObstaculosValidos: POV
-}

{- |
 Ponto de entrada dos testes da Tarefa 2.

 Quando executada utilizando @runTestTT@ irá executar os testes das categorias 'testsEM', 'testesPTV' e 'testesPOV'.
-}
testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ TestList [
        TestLabel "Testes EM" testesPTV,
        TestLabel "Testes PTV" testesPTV,
        TestLabel "Testes POV" testesPOV
    ]

{- |
 A função @genRandB100@ gera um número pseudo-aleatório entre 0 e 100.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
genRandB100 :: IO Int
genRandB100 = do
    x <- getTimeSA
    let rx = mod (head $ getPRand $ fromInteger x) 100

    if rx == 0 then pure 1 else pure rx

{- |
 A função @genTestCaseEM@ gera uma 'Assertion' para ser utilizado na função 'genTestEM'.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
genTestCaseEM s om = do
    r <- genRandB100 
    assertEM s om $ estendeMapa om r

{- |
 A função @genTestEM@ gera um 'TestCase' pré-formatado para a função 'estendeMapa'.
-}
genTestEM s om = s ~: TestCase $ genTestCaseEM s om

{- |
 A função @genCatchTestEM@ gera uma 'Assertion' para ser utilizado na função 'genCatchEM'.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
genCatchTestEM s om = TestCase $ genCatchEM s om

{- |
 A função @genCatchEM@ gera um 'TestCase' que verifica se um valor é uma excepção.
-}
genCatchEM s om = do
    result <- throws (genTestCaseEM s om)
    if result then pure () else assertFailure (s ++ "\nexpected: <exception>\n but got: ()")

{- |
 A função @testesEM@ testa a função 'estendeMapa' utilizando diferentes conjuntos de dados, válidos no contexto 
da função.
-}
testesEM = TestList [
        -- Testes envolvendo exclusivamente Rios
            -- Testes envolvendo largura 0
            genCatchTestEM "Teste 1 | Args: (Mapa 0 [])" (Mapa 0 []),
            genCatchTestEM "Teste 2 | Args: (Mapa 0 [(Rio 0,[])])" (Mapa 0 [(Rio 0,[])]),
            genCatchTestEM "Teste 3 | Args: (Mapa 0 [< 2x (Rio 0,[]) >])" (Mapa 0 [(Rio 0,[]),(Rio 0,[])]),
            genCatchTestEM "Teste 4 | Args: (Mapa 0 [< 3x (Rio 0,[]) >])" (Mapa 0 [(Rio 0,[]),(Rio 0,[]),(Rio 0,[])]),
            genCatchTestEM 
                "Teste 5 | Args: (Mapa 0 [< 4x (Rio 0,[]) >]) | Return: HUnitException" 
                (Mapa 0 [(Rio 0,[]),(Rio 0,[]),(Rio 0,[]),(Rio 0,[])]),
            genCatchTestEM 
                "Teste 6 | Args: (Mapa 0 [< 5x (Rio 0,[]) >]) | Return: HUnitException" 
                (Mapa 0 [(Rio 0,[]),(Rio 0,[]),(Rio 0,[]),(Rio 0,[]),(Rio 0,[])]),
        
            -- Testes envolvendo largura 2
                -- Testes envolvendo exclusivamente obstaculos Nenhum
                genTestEM "Teste 7 | Args: (Mapa 2 [])" (Mapa 2 []),
                genTestEM "Teste 8 | Args: (Mapa 2 [(Rio 0,[Nenhum,Nenhum])])" (Mapa 2 [(Rio 0,[Nenhum,Nenhum])]),
                genTestEM "Teste 9 | Args: (Mapa 2 [< 2x (Rio 0,[Nenhum,Nenhum]) >])" 
                    (Mapa 2 [(Rio 1,[Nenhum,Nenhum]),(Rio (-1),[Nenhum,Nenhum])]),
                genTestEM "Teste 10 | Args: (Mapa 2 [< 3x (Rio 0,[Nenhum,Nenhum]) >])" 
                    (Mapa 2 [(Rio 1,[Nenhum,Nenhum]),(Rio (-1),[Nenhum,Nenhum]),(Rio 0,[Nenhum,Nenhum])]),
                genCatchTestEM 
                    "Teste 11 | Args: (Mapa 2 [< 4x (Rio 0,[Nenhum,Nenhum]) >]) | Return: HUnitException" 
                    (Mapa 2 [(Rio 1,[Nenhum,Nenhum]),(Rio (-1),[Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum]),(Rio (-1),[Nenhum,Nenhum])]),
                genCatchTestEM 
                    "Teste 12 | Args: (Mapa 2 [< 5x (Rio 0,[Nenhum,Nenhum]) >]) | Return: HUnitException" 
                    (Mapa 2 [
                        (Rio 1,[Nenhum,Nenhum]),(Rio (-1),[Nenhum,Nenhum]),(Rio 1,[Nenhum,Nenhum]),(Rio (-1),[Nenhum,Nenhum]),
                        (Rio 1,[Nenhum,Nenhum])
                    ]),
                -- Testes envolvendo um obstáculo nulo e um obstáculo não-nulo
                genTestEM "Teste 13 | Args: (Mapa 2 [])" (Mapa 2 []),
                genTestEM "Teste 14 | Args: (Mapa 2 [(Rio 0,[Nenhum,Tronco])])" (Mapa 2 [(Rio 0,[Nenhum,Tronco])]),
                genTestEM "Teste 15 | Args: (Mapa 2 [< 2x (Rio 0,[Nenhum,Tronco]) >])" 
                    (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco])]),
                genTestEM "Teste 16 | Args: (Mapa 2 [< 3x (Rio 0,[Nenhum,Tronco]) >])" 
                    (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco]),(Rio 0,[Nenhum,Tronco])]),
                genCatchTestEM 
                    "Teste 17 | Args: (Mapa 2 [< 4x (Rio 0,[Nenhum,Tronco]) >]) | Return: HUnitException" 
                    (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco])]),
                genCatchTestEM 
                    "Teste 18 | Args: (Mapa 2 [< 5x (Rio 0,[Nenhum,Tronco]) >]) | Return: HUnitException" 
                    (Mapa 2 [
                        (Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco]),
                        (Rio 1,[Nenhum,Tronco])
                    ]),
        
        -- Testes envolvendo exclusivamente Estradas
            -- Testes envolvendo largura 0
            genCatchTestEM "Teste 19 | Args: (Mapa 0 [])" (Mapa 0 []),
            genCatchTestEM "Teste 20 | Args: (Mapa 0 [(Estrada 0,[])])" (Mapa 0 [(Estrada 0,[])]),
            genCatchTestEM "Teste 21 | Args: (Mapa 0 [< 2x (Estrada 0,[]) >])" (Mapa 0 [(Estrada 0,[]),(Estrada 0,[])]),
            genCatchTestEM "Teste 22 | Args: (Mapa 0 [< 3x (Estrada 0,[]) >])" (Mapa 0 [(Estrada 0,[]),(Estrada 0,[]),(Estrada 0,[])]),
            genCatchTestEM 
                "Teste 23 | Args: (Mapa 0 [< 4x (Estrada 0,[]) >]) | Return: HUnitException" 
                (Mapa 0 [(Estrada 0,[]),(Estrada 0,[]),(Estrada 0,[]),(Estrada 0,[])]),
            genCatchTestEM 
                "Teste 24 | Args: (Mapa 0 [< 5x (Estrada 0,[]) >]) | Return: HUnitException" 
                (Mapa 0 [(Estrada 0,[]),(Estrada 0,[]),(Estrada 0,[]),(Estrada 0,[]),(Estrada 0,[])]),
        
            -- Testes envolvendo largura 2
                -- Testes envolvendo exclusivamente obstaculos Nenhum
                genTestEM "Teste 25 | Args: (Mapa 2 [])" (Mapa 2 []),
                genTestEM "Teste 26 | Args: (Mapa 2 [(Estrada 0,[Nenhum,Nenhum])])" (Mapa 2 [(Estrada 0,[Nenhum,Nenhum])]),
                genTestEM "Teste 27 | Args: (Mapa 2 [< 2x (Estrada 0,[Nenhum,Nenhum]) >])" 
                    (Mapa 2 [(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum])]),
                genTestEM "Teste 28 | Args: (Mapa 2 [< 3x (Estrada 0,[Nenhum,Nenhum]) >])" 
                    (Mapa 2 [(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum])]),
                genTestEM 
                    "Teste 29 | Args: (Mapa 2 [< 4x (Estrada 0,[Nenhum,Nenhum]) >])" 
                    (Mapa 2 [(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum])]),
                genCatchTestEM 
                    "Teste 30 | Args: (Mapa 2 [< 5x (Estrada 0,[Nenhum,Nenhum]) >]) | Return: HUnitException" 
                    (Mapa 2 [
                        (Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Nenhum,Nenhum]),
                        (Estrada 0,[Nenhum,Nenhum])
                    ]),
                -- Testes envolvendo um obstáculo nulo e um obstáculo não-nulo
                genTestEM "Teste 31 | Args: (Mapa 2 [])" (Mapa 2 []),
                genTestEM "Teste 32 | Args: (Mapa 2 [(Estrada 0,[Nenhum,Carro])])" (Mapa 2 [(Estrada 0,[Nenhum,Carro])]),
                genTestEM "Teste 33 | Args: (Mapa 2 [< 2x (Estrada 0,[Nenhum,Carro]) >])" 
                    (Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro])]),
                genTestEM "Teste 34 | Args: (Mapa 2 [< 3x (Estrada 0,[Nenhum,Carro]) >])" 
                    (Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro])]),
                genTestEM 
                    "Teste 35 | Args: (Mapa 2 [< 4x (Estrada 0,[Nenhum,Carro]) >]) | Return: HUnitException" 
                    (Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro])]),
                genCatchTestEM 
                    "Teste 36 | Args: (Mapa 2 [< 5x (Estrada 0,[Nenhum,Carro]) >]) | Return: HUnitException" 
                    (Mapa 2 [
                        (Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro]),
                        (Estrada 0,[Nenhum,Carro])
                    ]),
    
        -- Testes envolvendo exclusivamente Relvas
            -- Testes envolvendo largura 0
            genCatchTestEM "Teste 37 | Args: (Mapa 0 [])" (Mapa 0 []),
            genCatchTestEM "Teste 38 | Args: (Mapa 0 [(Relva,[])])" (Mapa 0 [(Relva,[])]),
            genCatchTestEM "Teste 39 | Args: (Mapa 0 [< 2x (Relva,[]) >])" (Mapa 0 [(Relva,[]),(Relva,[])]),
            genCatchTestEM "Teste 40 | Args: (Mapa 0 [< 3x (Relva,[]) >])" (Mapa 0 [(Relva,[]),(Relva,[]),(Relva,[])]),
            genCatchTestEM 
                "Teste 41 | Args: (Mapa 0 [< 4x (Relva,[]) >]) | Return: HUnitException" 
                (Mapa 0 [(Relva,[]),(Relva,[]),(Relva,[]),(Relva,[])]),
            genCatchTestEM 
                "Teste 42 | Args: (Mapa 0 [< 5x (Relva,[]) >]) | Return: HUnitException" 
                (Mapa 0 [(Relva,[]),(Relva,[]),(Relva,[]),(Relva,[]),(Relva,[])]),
        
            -- Testes envolvendo largura 2
                -- Testes envolvendo exclusivamente obstaculos Nenhum
                genTestEM "Teste 43 | Args: (Mapa 2 [])" (Mapa 2 []),
                genTestEM "Teste 44 | Args: (Mapa 2 [(Relva,[Nenhum,Nenhum])])" (Mapa 2 [(Relva,[Nenhum,Nenhum])]),
                genTestEM "Teste 45 | Args: (Mapa 2 [< 2x (Relva,[Nenhum,Nenhum]) >])" 
                    (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum])]),
                genTestEM "Teste 46 | Args: (Mapa 2 [< 3x (Relva,[Nenhum,Nenhum]) >])" 
                    (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum])]),
                genTestEM 
                    "Teste 47 | Args: (Mapa 2 [< 4x (Relva,[Nenhum,Nenhum]) >])" 
                    (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum])]),
                genCatchTestEM 
                    "Teste 48 | Args: (Mapa 2 [< 5x (Relva,[Nenhum,Nenhum]) >]) | Return: HUnitException" 
                    (Mapa 2 [
                        (Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum]),
                        (Relva,[Nenhum,Nenhum])
                    ]),
                -- Testes envolvendo um obstáculo nulo e um obstáculo não-nulo
                genTestEM "Teste 49 | Args: (Mapa 2 [])" (Mapa 2 []),
                genTestEM "Teste 50 | Args: (Mapa 2 [(Relva,[Nenhum,Arvore])])" (Mapa 2 [(Relva,[Nenhum,Arvore])]),
                genTestEM "Teste 51 | Args: (Mapa 2 [< 2x (Relva,[Nenhum,Arvore]) >])" 
                    (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore])]),
                genTestEM "Teste 52 | Args: (Mapa 2 [< 3x (Relva,[Nenhum,Arvore]) >])" 
                    (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore])]),
                genTestEM 
                    "Teste 53| Args: (Mapa 2 [< 4x (Relva,[Nenhum,Arvore]) >]) | Return: HUnitException" 
                    (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore])]),
                genCatchTestEM 
                    "Teste 54 | Args: (Mapa 2 [< 5x (Relva,[Nenhum,Arvore]) >]) | Return: HUnitException" 
                    (Mapa 2 [
                        (Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),
                        (Relva,[Nenhum,Arvore])
                    ])
    
    ]

{- |
 A função @testesPTV@ testa a função 'proximosTerrenosValidos' utilizando diferentes conjuntos de dados, válidos no contexto 
da função.

 == Observações
 A largura dos mapas é assumida como 0 para todos os testes, dado que tanto a largura como os obstáculos das linhas 
existentes no mapa são ignoradas pela função.
-}
testesPTV = test [
    -- Testes envolvendo mapas vazios
    "Teste 1 | Args: (Mapa 0 [])" ~: [Rio 0,Estrada 0,Relva] ~=? proximosTerrenosValidos (Mapa 0 []),
    "Teste 2 | Args: (Mapa 2 [])" ~: [Rio 0,Estrada 0,Relva] ~=? proximosTerrenosValidos (Mapa 2 []),

    -- Testes envolvendo mapas de largura 0 com apenas Rios
    "Teste 3 | Args: (Mapa 0 [(Rio 0, [])])" -- Mapa contendo um único Rio.
        ~: [Rio 0,Estrada 0,Relva] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Rio 0, [])]),
    "Teste 4 | Args: (Mapa 0 [(Rio 0, []),(Rio 0, []),(Rio 0, [])])" -- Mapa contendo (limite - 1) Rios.
        ~: [Rio 0,Estrada 0,Relva] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Rio 0, []),(Rio 0, []),(Rio 0, [])]),
    "Teste 5 | Args: (Mapa 0 [(Rio 0, []),(Rio 0, []),(Rio 0, []),(Rio 0, [])])" -- Mapa contendo (limite) Rios.
        ~: [Estrada 0,Relva] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Rio 0, []),(Rio 0, []),(Rio 0, []),(Rio 0, [])]),

    -- Testes envolvendo mapas de largura 0 com apenas Estradas
    "Teste 6 | Args: (Mapa 0 [(Estrada 0, [])])" -- Mapa contendo uma única Estrada.
        ~: [Rio 0,Estrada 0,Relva] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Estrada 0, [])]),
    "Teste 7 | Args: (Mapa 0 [(Estrada 0, []),(Estrada 0, []),(Estrada 0, []),(Estrada 0, [])])" -- Mapa contendo (limite - 1) Estradas.
        ~: [Rio 0,Estrada 0,Relva] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Estrada 0, []),(Estrada 0, []),(Estrada 0, []),(Estrada 0, [])]),
    "Teste 8 | Args: (Mapa 0 [(Estrada 0, []),(Estrada 0, []),(Estrada 0, []),(Estrada 0, []),(Estrada 0, [])])" -- Mapa contendo (limite) Estrada.
        ~: [Rio 0,Relva] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Estrada 0, []),(Estrada 0, []),(Estrada 0, []),(Estrada 0, []),(Estrada 0, [])]),

    -- Testes envolvendo mapas de largura 0 com apenas Estradas
    "Teste 9 | Args: (Mapa 0 [(Relva, [])])" -- Mapa contendo uma única Relva.
        ~: [Rio 0,Estrada 0,Relva] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Relva, [])]),
    "Teste 10 | Args: (Mapa 0 [(Relva, []),(Relva, []),(Relva, []),(Relva, [])])" -- Mapa contendo (limite - 1) Relvas.
        ~: [Rio 0,Estrada 0,Relva] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Relva, []),(Relva, []),(Relva, []),(Relva, [])]),
    "Teste 11 | Args: (Mapa 0 [(Relva, []),(Relva, []),(Relva, []),(Relva, []),(Relva, [])])" -- Mapa contendo (limite) Relvas.
        ~: [Rio 0,Estrada 0] 
        ~=? proximosTerrenosValidos (Mapa 0 [(Relva, []),(Relva, []),(Relva, []),(Relva, []),(Relva, [])])
    ]

{- |
 A função @testesPOV@ testa a função 'proximosObstaculosValidos' utilizando diferentes conjuntos de dados, válidos no 
contexto da função.

 == Observações
 A ordem de colocação dos obstáculos na lista de obstáculos é ignorada, dado que nenhum mecanismo presente nesta função ou
nas suas dependentes requer uma ordem específica dos elementos nessa mesma lista.
-}
testesPOV = test [
    -- Testes envolvendo terrenos vazios
    -- Testes envolvendo largura nula (UNIVERSAL PARA TODO O NÚMERO DE OBSTÁCULOS)
    "Teste 1 | Args: 0 (Rio 0,[])" ~: [] ~=? proximosObstaculosValidos 0 (Rio 0, []),
    "Teste 2 | Args: 0 (Estrada 0,[])" ~: [] ~=? proximosObstaculosValidos 0 (Estrada 0, []),
    "Teste 3 | Args: 0 (Relva,[])" ~: [] ~=? proximosObstaculosValidos 0 (Relva, []),

    -- Testes envolvendo largura 1
    "Teste 4 | Args: 1 (Rio 0,[])" ~: [Nenhum] ~=? proximosObstaculosValidos 1 (Rio 0, []),
    "Teste 5 | Args: 1 (Estrada 0,[])" ~: [Nenhum] ~=? proximosObstaculosValidos 1 (Estrada 0, []),
    "Teste 6 | Args: 1 (Relva,[])" ~: [Nenhum] ~=? proximosObstaculosValidos 1 (Relva, []),

    -- Testes envolvendo largura 2
    "Teste 7 | Args: 2 (Rio 0,[])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 2 (Rio 0, []),
    "Teste 8 | Args: 2 (Estrada 0,[])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 2 (Estrada 0, []),
    "Teste 9 | Args: 2 (Relva,[])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 2 (Relva, []),

    -- Testes envolvendo largura 5
    "Teste 10 | Args: 5 (Rio 0,[])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 5 (Rio 0, []),
    "Teste 11 | Args: 5 (Estrada 0,[])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 0, []),
    "Teste 12 | Args: 5 (Relva,[])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, []),

    -- Testes envolvendo terrenos com um único obstáculo
    -- Testes envolvendo largura 2 e o obstáculo Nenhum
    "Teste 13 | Args: 2 (Rio 0,[Nenhum])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 2 (Rio 0, [Nenhum]),
    "Teste 14 | Args: 2 (Estrada 0,[Nenhum])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 2 (Estrada 0, [Nenhum]),
    "Teste 15 | Args: 2 (Relva,[Nenhum])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 2 (Relva, [Nenhum]),

    -- Testes envolvendo largura 2 e o obstáculo não-nulo de cada terreno
    "Teste 16 | Args: 2 (Rio 0,[Tronco])" ~: [Nenhum] ~=? proximosObstaculosValidos 2 (Rio 0, [Tronco]),
    "Teste 17 | Args: 2 (Estrada 0,[Carro])" ~: [Nenhum] ~=? proximosObstaculosValidos 2 (Estrada 0, [Carro]),
    "Teste 18 | Args: 2 (Relva,[Arvore])" ~: [Nenhum] ~=? proximosObstaculosValidos 2 (Relva, [Arvore]),

    -- Testes envolvendo largura 5 e o obstáculo Nenhum
    "Teste 19 | Args: 5 (Rio 0,[Nenhum])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 5 (Rio 0, [Nenhum]),
    "Teste 20 | Args: 5 (Estrada 0,[Nenhum])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 0, [Nenhum]),
    "Teste 21 | Args: 5 (Relva,[Nenhum])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, [Nenhum]),

    -- Testes envolvendo largura 5 e o obstáculo não-nulo de cada terreno
    "Teste 22 | Args: 5 (Rio 0,[Tronco])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 5 (Rio 0, [Tronco]),
    "Teste 23 | Args: 5 (Estrada 0,[Carro])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 0, [Carro]),
    "Teste 24 | Args: 5 (Relva,[Arvore])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, [Arvore]),

    -- Testes envolvendo terrenos com dois obstáculos
    -- Testes envolvendo largura 2 e dois obstáculos Nenhum
    "Teste 25 | Args: 2 (Rio 0,[Nenhum,Nenhum])" ~: [] ~=? proximosObstaculosValidos 2 (Rio 0, [Nenhum,Nenhum]),
    "Teste 26 | Args: 2 (Estrada 0,[Nenhum,Nenhum])" ~: [] ~=? proximosObstaculosValidos 2 (Estrada 0, [Nenhum,Nenhum]),
    "Teste 27 | Args: 2 (Relva,[Nenhum,Nenhum])" ~: [] ~=? proximosObstaculosValidos 2 (Relva, [Nenhum,Nenhum]),

    -- Testes envolvendo largura 2 e dois obstáculos diferentes
    "Teste 28 | Args: 2 (Rio 0,[Nenhum,Tronco])" ~: [] ~=? proximosObstaculosValidos 2 (Rio 0, [Nenhum,Tronco]),
    "Teste 29 | Args: 2 (Estrada 0,[Nenhum,Carro])" ~: [] ~=? proximosObstaculosValidos 2 (Estrada 0, [Nenhum,Carro]),
    "Teste 30 | Args: 2 (Relva,[Nenhum,Arvore])" ~: [] ~=? proximosObstaculosValidos 2 (Relva, [Nenhum,Arvore]),

    -- Testes envolvendo largura 2 e dois obstáculos não-nulos
    "Teste 31 | Args: 2 (Rio 0,[Tronco,Tronco])" ~: [] ~=? proximosObstaculosValidos 2 (Rio 0, [Tronco,Tronco]),
    "Teste 32 | Args: 2 (Estrada 0,[Carro,Carro])" ~: [] ~=? proximosObstaculosValidos 2 (Estrada 0, [Carro,Carro]),
    "Teste 33 | Args: 2 (Relva,[Arvore,Arvore])" ~: [] ~=? proximosObstaculosValidos 2 (Relva, [Arvore,Arvore]),

    -- Testes envolvendo largura 5 e dois obstáculos Nenhum
    "Teste 34 | Args: 5 (Rio 0,[Nenhum,Nenhum])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 5 (Rio 0, [Nenhum,Nenhum]),
    "Teste 35 | Args: 5 (Estrada 0,[Nenhum,Nenhum])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 0, [Nenhum,Nenhum]),
    "Teste 36 | Args: 5 (Relva,[Nenhum,Nenhum])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, [Nenhum,Nenhum]),

    -- Testes envolvendo largura 5 e dois obstáculos diferentes
    "Teste 37 | Args: 5 (Rio 0,[Nenhum,Tronco])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 5 (Rio 0, [Nenhum,Tronco]),
    "Teste 38 | Args: 5 (Estrada 0,[Nenhum,Carro])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 0, [Nenhum,Carro]),
    "Teste 39 | Args: 5 (Relva,[Nenhum,Arvore])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, [Nenhum,Arvore]),

    -- Testes envolvendo largura 5 e dois obstáculos não-nulos
    "Teste 40 | Args: 5 (Rio 0,[Tronco,Tronco])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 5 (Rio 0, [Tronco,Tronco]),
    "Teste 41 | Args: 5 (Estrada 0,[Carro,Carro])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 0, [Carro,Carro]),
    "Teste 42 | Args: 5 (Relva,[Arvore,Arvore])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, [Arvore,Arvore]),

    -- Testes envolvendo mais que dois obstáculos
    -- Testes envolvendo largura 5 e 4 obstáculos Nenhum
    "Teste 43 | Args: 5 (Rio 0,[Nenhum,Nenhum,Nenhum,Nenhum])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 5 (Rio 0, [Nenhum,Nenhum,Nenhum,Nenhum]),
    "Teste 44 | Args: 5 (Estrada 0,[Nenhum,Nenhum,Nenhum,Nenhum])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 0, [Nenhum,Nenhum,Nenhum,Nenhum]),
    "Teste 45 | Args: 5 (Relva,[Nenhum,Nenhum,Nenhum,Nenhum])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, [Nenhum,Nenhum,Nenhum,Nenhum]),

    -- Testes envolvendo largura 5 e dois pares de obstáculos diferentes
    "Teste 46 | Args: 5 (Rio 0,[Nenhum,Tronco,Nenhum,Tronco])" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 5 (Rio 0, [Nenhum,Tronco,Nenhum,Tronco]),
    "Teste 47 | Args: 5 (Estrada 0,[Nenhum,Carro,Nenhum,Carro])" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 0, [Nenhum,Carro,Nenhum,Carro]),
    "Teste 48 | Args: 5 (Relva,[Nenhum,Arvore,Nenhum,Arvore])" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, [Nenhum,Arvore,Nenhum,Arvore]),

    -- Testes envolvendo largura 5 e 4 obstáculos não-nulos
    "Teste 49 | Args: 5 (Rio 0,[Tronco,Tronco,Tronco,Tronco])" ~: [Nenhum] ~=? proximosObstaculosValidos 5 (Rio 0, [Tronco,Tronco,Tronco,Tronco]),
    "Teste 50 | Args: 5 (Estrada 0,[Carro,Carro,Carro,Carro])" ~: [Nenhum] ~=? proximosObstaculosValidos 5 (Estrada 0, [Carro,Carro,Carro,Carro]),
    "Teste 51 | Args: 5 (Relva,[Arvore,Arvore,Arvore,Arvore])" ~: [Nenhum] ~=? proximosObstaculosValidos 5 (Relva, [Arvore,Arvore,Arvore,Arvore]),

    -- Testes envolvendo largura 5 e 4 obstáculos não-nulos + 1 obstáculo nulo
    "Teste 52 | Args: 5 (Rio 0,[Tronco,Tronco,Tronco,Tronco,Nenhum])" ~: [] ~=? proximosObstaculosValidos 5 (Rio 0, [Tronco,Tronco,Tronco,Tronco,Nenhum]),
    "Teste 53 | Args: 5 (Estrada 0,[Carro,Carro,Carro,Carro,Nenhum])" ~: [] ~=? proximosObstaculosValidos 5 (Estrada 0, [Carro,Carro,Carro,Carro,Nenhum]),
    "Teste 54 | Args: 5 (Relva,[Arvore,Arvore,Arvore,Arvore,Nenhum])" ~: [] ~=? proximosObstaculosValidos 5 (Relva, [Arvore,Arvore,Arvore,Arvore,Nenhum]),
    
    -- Testes envolvendo largura 5 e 5 obstáculos não-nulos
    "Teste 55 | Args: 5 (Rio 0,[Tronco,Tronco,Tronco,Tronco,Tronco])" ~: [] ~=? proximosObstaculosValidos 5 (Rio 0, [Tronco,Tronco,Tronco,Tronco,Tronco]),
    "Teste 56 | Args: 5 (Estrada 0,[Carro,Carro,Carro,Carro,Tronco])" ~: [] ~=? proximosObstaculosValidos 5 (Estrada 0, [Carro,Carro,Carro,Carro,Carro]),
    "Teste 57 | Args: 5 (Relva,[Arvore,Arvore,Arvore,Arvore,Tronco])" ~: [] ~=? proximosObstaculosValidos 5 (Relva, [Arvore,Arvore,Arvore,Arvore,Arvore])
    ]

{- |
    __/TODO:/__ Refactor this monstrosity.

    A função @assertEM@ é uma função de testes cujo papel é exclusivamente o teste da função @extendeMapa@.
    
    Caso o teste falhe, um dos erros da lista abaixo será exibido. Os erros são escritos em inglês para manter a consistência
com os erros do HUnit, dado que esta função é suposto ser executada no contexto do mesmo.

    == Lista de Erros
        - @E001@: O Mapa atual está vazio.  
        - @E002@: O Mapa resultante é inválido.  
        - @E003@: O Mapa atual não possuí as linhas do Mapa original. 
        - @E004@: O Mapa resultante não possuí um caminho válido. 
-}
assertEM :: String -> Mapa -> Mapa -> Assertion
assertEM preface om@(Mapa w lo) am@(Mapa aw alo) 
    | null alo = assertFailure "<E001> empty map"
    | otherwise = if vl == "" then pure () else assertFailure $ msgPreface ++ vl
    where
        (oflt,oflo) = head lo
        (aflt,aflo) = head alo

        -- Terrenos Válidos para a próxima linha
        pt = proximosTerrenosValidos om
        -- Os obstáculos válidos para cada tipo de terreno.
        po = map (\x -> (x, proximosObstaculosValidos w (x,[]))) pt
        -- Lista de todos os índices dos tiles vazios da linha anterior á gerada.
        i = map snd $ filterIndexed (\x _ -> x == Nenhum) (if null lo then map (const Nenhum) [1..w] else oflo)
        
        __TRUE = ""
        vl = 
            -- Mapa atual é válido
            if mapaValido am then
                -- Mapa atual contém todas linhas do Mapa original
                if tail alo == lo then
                    -- O Mapa atual possuí pelo menos um caminho válido
                    if 
                        length alo == 1
                        || (
                            getTerrenoConstr aflt == 0 
                            && getTerrenoConstr oflt == 0 
                            && anyIndexed (\(x, i) -> x == Tronco && oflo !! i == Tronco) aflo
                        )
                        || (
                            getTerrenoConstr aflt /= 0 
                            && getTerrenoConstr oflt == 0 
                            && anyIndexed (\(x, i) -> x == Nenhum && oflo !! i == Tronco) aflo
                        )
                        || (
                            getTerrenoConstr aflt /= 0 
                            && getTerrenoConstr oflt /= 0 
                            && anyIndexed (\(x, i) -> x == Nenhum && oflo !! i == Nenhum) aflo
                        )
                    then __TRUE
                    else "<E004> impossible progression: no valid path between line 0 and line 1"
                else "<E003> mismatched history"
            else "<E002> invalid map: " ++ show am

        msgPreface = if null preface then "" else preface ++ "\n"

