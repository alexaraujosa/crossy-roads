{- |
Module      : Render.World
Description : Módulo de manipulação de Mundos
Copyright   : Alex Araújo de Sá <a104257@alunos.uminho.pt>
              Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de manipulação de Mundos. Permite carregar, salvar e criar Mundos.
-}

{-# LANGUAGE DuplicateRecordFields #-}

module Render.World where
import LI12223
import Render.Play.Internal
import System.Random
import Locale (en_US)
import Util.Logger (defaultLogData, loggerMixed)
import Util (getDate)

-- | Representa o mapa inicial.
defaultWorld :: 
    Int     -- ^ A largura do Mapa.
    -> Int  -- ^ O número de linhas
    -> Map  -- ^ O Mapa contendo linhas de relva vazias - /grace space/
defaultWorld w l = Map w 3 $ map (const (Relva, map (const Nenhum) [1..w])) [1..l]

