{- |
Module      : Tarefa4_2022li1g003
Description : Determinar se o jogo terminou
Copyright   : Alex Araújo de Sá  <a104257@alunos.uminho.pt>
              Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g003 where

import LI12223
import Tarefa3_2022li1g003

-- TODO: Executar quando o jogador morre - 2a Fase
-- die = undefined

{- |
 A função @jogoTerminou@ determina se o 'Jogador' perdeu o jogo, isto é, se está "debaixo" de um carro, fora do mapa ou dentro de água.
 -} 
jogoTerminou :: 
    Jogo -- ^ Jogo a ser considerado.
    -> Bool -- ^ Resultado da verificação.
jogoTerminou (Jogo (Jogador (x, y)) (Mapa w l)) 
    | x >= maxX || x < 0 = True -- Caso o jogador esteja fora do mapa no eixo X.
    | y >= maxY || y < 0 = True -- Caso o jogador esteja fora do mapa no eixo Y.
    | isAgua (l !! y) x = True -- Caso o jogador se encontre dentro de água.
    | isCarro (l !! y) x = True -- Caso o jogador se encontre "debaixo" de um carro.
    | isArvore (l !! y) x = True
    | otherwise = False
        
    where maxX = w
          maxY = length l   
