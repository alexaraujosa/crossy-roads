{- |
Module      : Tarefa5_v2
Description : Desliza o jogo.
Copyright   : Alex Araújo de Sá  <a104257@alunos.uminho.pt>
              Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo para a realização da reestruturação da Tarefa5, dado novo tipo de Jogo (Game).
-}
module Tarefa5_v2 where

import Render.Play.Internal ( Player(Player), Game(..), Map(Map) )
import Render.World.Level (getGameMap, setGameMap, killPlayer, getGame, getGamePlayerCoords, isInGame, setPlayerCoords, getGamePlayerOffset, setPlayerOffset, setPlayer, getGamePlayer, setPlayerVelocity, getGamePlayerVelocity, setGame)
import Data.Maybe (isNothing, fromJust)
import Util (getTerrenoConstr, shiftN, getTerrenoVel)
import Tarefa2_v2 ( estendeMapa )

{- |
 A função @deslizaGame@ gera uma linha no topo do mapa, removendo a última, assim mantendo a quantidade de linhas de mapa.
 -} 
deslizaGame :: 
    Int -- ^ Número a ser considerado.
    -> Int -- ^ Número a ser considerado.
    -> Game -- ^ Jogo a ser considerado.
    -> Game -- ^ Jogo com Mapa e Coordenadas do Jogador atualizadas.
deslizaGame n num (Game (Player offs v (x,y)) (Map w o l))  | not (null l) = Game (Player offs v (x,y+1)) (estendeMapa (Map w o (init l)) n num)  
