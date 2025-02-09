{- |
Module      : Tarefa5_2022li1g003
Description : Deslizar o jogo
Copyright   : Alex Araújo de Sá  <a104257@alunos.uminho.pt>
              Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g003 where

import LI12223
import Tarefa2_2022li1g003 (estendeMapa)

{- |
 A função @deslizaJogo@ gera uma nova linha (aleatória) no topo do mapa através de um número, apagando a última, bem como aumenta a coordenada 'y' do @Jogador@ em 1 unidade, assim criando um efeito de 'deslize'.
 
 __AVISO:__ O tamanho do mapa deverá ter, pelo menos, uma linha.
 -} 
deslizaJogo :: 
    Int -- ^ Número a ser considerado.
    -> Jogo -- ^ Jogo a ser considerado.
    -> Jogo -- ^ Jogo com Mapa e Coordenadas do Jogador atualizadas.
deslizaJogo n (Jogo (Jogador (x,y)) m@(Mapa w l)) | not (null l) = Jogo (Jogador (x,y+1)) (estendeMapa (Mapa w (init l)) n)  