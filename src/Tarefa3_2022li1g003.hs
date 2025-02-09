{- |
Module      : Tarefa3_2022li1g003
Description : Movimentação do personagem e obstáculos
Copyright   : Alex Araújo de Sá  <a104257@alunos.uminho.pt>
              Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g003 where

import LI12223
import Tarefa2_2022li1g003
import Util

{- |
 A função @animaJogo@ movimenta os obstáculos (de acordo com a 'Velocidade' do terreno em que se encontram, e o personagem, de acordo com a 'Jogada' dada.

 Assume-se que o Mapa seja fornecido através de uma função futuramente implementada que garantirá que a posição inicial do
Jogador é válida.
-}
animaJogo :: 
    Jogo -- ^ Jogo a ser considerado.
    -> Jogada -- ^ Ação tomada pelo 'Jogador'. 
    -> Jogo -- ^ Jogo atualizado após a 'Jogada'.
animaJogo (Jogo (Jogador (x, y)) (Mapa w l)) movi = Jogo mov (Mapa w (map (moverObstaculos mov) l))
    where
        mov = posicao (Jogo (Jogador (x, y)) (Mapa w l)) movi
        --if movi == Parado then j else j--corretorPosicao j

{- |
 A função @posicao@ determina a posição em que ficará o 'Jogador' após uma 'Jogada'.

  __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
posicao :: 
    Jogo -- ^ Jogo a ser considerado.
    -> Jogada -- Ação tomada pelo 'Jogador'.
    -> Jogador -- ^ Posição atualizada após a 'Jogada'.
posicao j@(Jogo (Jogador (x, y)) (Mapa w l)) Parado -- Posição do jogador se ficar 'Parado'.
    | getTerrenoConstr clt == 0 = let (Jogo jog _) = attemptMove j X (getTerrenoVel clt) in jog -- Movimentação do jogador se ele se encontrar numa linha @Rio@.
    | otherwise = Jogador (x, y) 
    where
        clt = fst $ l !! y
        maxX = w
        maxY = length l

posicao j@(Jogo (Jogador (x, y)) (Mapa w l)) (Move d) = case d of { -- Posição do jogador se ele se movimentar.
    Cima -> let (Jogo jog _) = if isPositionValid (Mapa w l) (x, y-1) then attemptMove j Y (-1) else j in jog;
    Baixo -> let (Jogo jog _) = if isPositionValid (Mapa w l) (x, y+1) then attemptMove j Y 1 else j in jog;
    Direita -> let (Jogo jog _) = if isPositionValid (Mapa w l) (x+1, y) then attemptMove j X 1 else j in jog; 
    Esquerda -> let (Jogo jog _) = if isPositionValid (Mapa w l) (x-1, y) then attemptMove j X (-1) else j in jog;
}

{- |
 A função @isPositionValid@ determina se uma posição é válida, ou seja, se nessa posição não existe uma árvore ou um carro, ou se existe um tronco.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
 -} 
isPositionValid :: 
    Mapa -- ^ Mapa a ser considerado.
    -> Coordenadas -- ^ Par de coordenadas a ser considerado.
    -> Bool -- ^ Resultado da verificação.
isPositionValid (Mapa w l) (x,y) = (x >= 0 && x < maxX && y >= 0 && y < maxY) && not (isArvore (l !! y) x || isCarro (l !! y) x) -- || isAgua (l !! y) x
  where
    maxX = w
    maxY = length l

 {- |
 A função @isArvore@ determina se numa linha de 'Relva', numa determinada coordenada, existe uma árvore.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
 -} 
isArvore :: 
    (Terreno, [Obstaculo]) -- ^ Linha a ser considerada.
    -> Int -- ^ Coordenada 'x' a ser considerada.
    -> Bool -- ^ Resultado da verificação.
isArvore (Relva, l) n = length l > n && l !! n == Arvore
isArvore _ _ = False

 {- |
 A função @isAgua@ determina se numa linha de 'Rio', numa determinada coordenada, não existe um tronco.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
 -} 
isAgua :: 
    (Terreno, [Obstaculo]) -- ^ Linha a ser considerada.
    -> Int -- ^ Coordenada 'x' a ser considerada.
    -> Bool -- ^ Resultado da verificação.
isAgua (Rio _, l) n = length l > n && l !! n == Nenhum
isAgua _ _ = False

 {- |
 A função @isCarro@ determina se numa linha de 'Estrada', numa determinada coordenada, existe um carro.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
 -} 
isCarro :: 
    (Terreno, [Obstaculo]) -- ^ Linha a ser considerada.
    -> Int -- ^ Coordenada 'x' a ser considerada.
    -> Bool -- ^ Resultado da verificação.
isCarro (Estrada _, l) n = length l > n && l !! n == Carro
isCarro _ _ = False

data Axis = X | Y deriving (Show, Read, Eq) -- ^ Axis representa a movimentação do 'Jogador' no eixo X ou Y. 
 {- |
 A função @attemptMove@ determina se uma 'Jogada' pode ser, ou não, efetuada, efetuando-a se for possível.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
 -} 
attemptMove :: 
    Jogo -- ^ Jogo a ser considerado.
    -> Axis -- ^ Eixo a ser considerado.
    -> Int -- ^ Quantidade de unidades a se mover no Eixo.
    -> Jogo -- ^ Jogo atualizado após a 'Jogada'.
attemptMove j@(Jogo (Jogador (x, y)) (Mapa w l)) a n 
    | x > maxX || x < 0 = j -- Caso a posição do jogador seja fora do mapa.
    | y > maxY || y < 0 = j -- Caso a posição do jogador seja fora do mapa.
    | otherwise = case a of {
        X -> if maxX >= x + n && x + n >= 0 then Jogo (Jogador (x + n, y)) (Mapa w l) else j;
        Y -> if maxY >= y + n && y + n >= 0 then Jogo (Jogador (x, y + n)) (Mapa w l) else j;
    }
    where
        maxX = w - 1
        maxY = length l - 1

{- |
 A função @moverObstaculos@ desloca os obstáculos de uma linha consoante a velocidade do Terreno.
 Caso um obstáculo desapareça de um lado do mapa, este irá reaparecer no outro lado.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
-- moverObstaculos :: 
--     (Terreno, [Obstaculo]) -- ^ Linha de Mapa a ser considerada.
--     -> (Terreno, [Obstaculo]) -- ^ Linha de Mapa com os objetos deslocados.
-- moverObstaculos (Rio v, l) = (Rio v, shiftN v l) -- Deslocação de obstáculos |v| unidades, de uma linha de terreno @Rio@.
-- moverObstaculos (Estrada v, l) = (Estrada v, shiftN v l) -- Deslocação de obstáculos |v| unidades, de uma linha de terreno @Estrada@.
-- moverObstaculos (Relva, l) = (Relva, l) -- Deslocação de obstáculos nula, devido a se tratar de uma linha de terreno @Relva@.


moverObstaculos :: 
    Jogador                   -- ^ O Jogador atual
    -> (Terreno, [Obstaculo]) -- ^ Linha de Mapa a ser considerada.
    -> (Terreno, [Obstaculo]) -- ^ Linha de Mapa com os objetos deslocados.
moverObstaculos (Jogador (jx,_)) (t,ol)
    | getTerrenoConstr t == 0  = (t, map fst rOL0)
    | getTerrenoConstr t == 1 || getTerrenoConstr t == 2 = (t, map fst rOL12)
    | otherwise = (t,ol)
    where
        v = getTerrenoVel t
        rOL12 = shiftNCI (\l -> if fst (l !! jx) /= Nenhum then 1 else 0) v ol
        rOL0 = shiftNCI (\l -> if fst (l !! jx) /= Tronco then 1 else 0) v ol
