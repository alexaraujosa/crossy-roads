{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{- |
Module      : Render.World.Level
Description : Módulo com funções auxiliares.
Copyright   : Alex Araújo de Sá <a104257@alunos.uminho.pt>
              Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo que contém as funções para o Jogo ativo.
-}
module Render.World.Level where

import Render.Play.Internal
import LI12223
import Data.Maybe (isNothing, fromJust)
import Locale (getLocalizedString)
import Data.List (elemIndex)
import Util (replaceAt)

{-
=======================================
            ROOT GETTERS               
=======================================
-}
-- | Obtém os dados do Jogo atual.
getGame :: GameState -> Maybe Game
getGame gs
    | isNothing ld = Nothing
    | otherwise = Just $ game $ fromJust ld
    where
        ld = levelData gs

-- | Obtém os dados do Jogador.
getGamePlayer :: GameState -> Maybe Player
getGamePlayer gs
    | isNothing g = Nothing
    | otherwise = Just p
    where
        g = getGame gs
        (Game p _) = fromJust g

-- | Obtém os dados relacionados com o Jogador neste nível.
getPlayerData :: GameState -> Maybe LocalPlayerData
getPlayerData gs
    | isNothing ld = Nothing
    | otherwise = Just $ playerData $ fromJust ld
    where
        ld = levelData gs

-- | Determina se o jogo está pausado.
isPaused :: GameState -> Bool
isPaused gs
    | isNothing ld = False
    | otherwise = paused $ fromJust ld
    where
        ld = levelData gs

-- | Determina se o Jogador morreu.
isDead :: GameState -> Bool
isDead gs
    | isNothing ld = False
    | otherwise = killed $ fromJust ld
    where
        ld = levelData gs

-- | Determina se o Jogador se encontra num menu.
isInMenu :: GameState -> Bool
isInMenu gs = inMenu (state gs) || inGameMenu (state gs)--Render.Play.Internal.inMenu $ state gs

-- | Determina se o Jogador se encontra num jogo.
isInGame :: GameState -> Bool
isInGame gs = Render.Play.Internal.inGame $ state gs

-- | Determina se o Jogador encontra-se num menu dentro de um jogo.
isInGameMenu :: GameState -> Bool
isInGameMenu gs = Render.Play.Internal.inGameMenu $ state gs

{-
=======================================
            GAME GETTERS               
=======================================
-}

-- | Obtém o Mapa atual do jogo.
getGameMap :: GameState -> Maybe Map
getGameMap gs
    | isNothing game = Nothing
    | otherwise = Just m
    where
        game = getGame gs
        (Game _ m) = fromJust game

-- | Obtém uma linha do Mapa atual.
getGameLine :: GameState -> Int -> Maybe Line
getGameLine gs i 
    | isNothing game = Nothing
    | otherwise = Just $ ll !! i
    where
        game = getGame gs
        (Game _ (Map _ _ ll)) = fromJust game

-- | Obtém a largura do Mapa atual.
getGameWidth :: GameState -> Maybe Int
getGameWidth gs
    | isNothing game = Nothing
    | otherwise = Just w
    where
        game = getGame gs
        (Game _ (Map w _ _)) = fromJust game

{-
=======================================
        PLAYER DATA GETTERS            
=======================================
-}

-- | Obtém as coordenadas exatas do Jogador no mapa atual.
getGamePlayerCoords :: GameState -> Maybe Coordinates
getGamePlayerCoords gs 
    | isNothing game = Nothing
    | otherwise = Just c
    where
        game = getGame gs
        (Game (Player _ _ c) _) = fromJust game

-- | Obtém o offset do Mapa atual.
getGamePlayerOffset :: GameState -> Maybe MapOffset
getGamePlayerOffset gs
    | isNothing game = Nothing
    | otherwise = Just off
    where
        game = getGame gs
        (Game (Player off _ _) _) = fromJust game

-- | Obtém a velocidade do jogador.
getGamePlayerVelocity :: GameState -> Maybe Velocity
getGamePlayerVelocity gs
    | isNothing game = Nothing
    | otherwise = Just v
    where
        game = getGame gs
        (Game (Player _ v _) _) = fromJust game

-- | Obtém as coordenadas base do Jogador.
getDisplayPlayerCoords :: GameState -> Maybe Coordenadas
getDisplayPlayerCoords gs
    | isNothing pc = Nothing
    | otherwise = let (x,y) = fromJust pc in Just (floor x, floor y)
    where
        pc = getGamePlayerCoords gs

-- | Obtém os pontos do Jogador no nível atual.
getPlayerLevelPoints :: GameState -> Maybe Int
getPlayerLevelPoints gs 
    | isNothing lpd = Nothing
    | otherwise = Just p
    where
        lpd = getPlayerData gs
        (LocalPlayerData p) = fromJust lpd

{-
=======================================
            ROOT SETTERS               
=======================================
-}

-- | Modifica a estrutura Jogo para o nível atual.
setGame :: GameState -> Game -> Maybe GameState
setGame gs g
    | isNothing ld = Nothing
    | otherwise = Just $ gs { levelData = Just ((fromJust ld) { game = g }) }
    where
        ld = levelData gs

-- | Modifica o Mapa para o nível atual.
setGameMap :: GameState -> Map -> Maybe GameState
setGameMap gs m
    | isNothing ld = Nothing
    | otherwise = Just $ gs { levelData = Just ((fromJust ld) { game = Game p m }) }
    where
        ld = levelData gs
        (Game p _) = game $ fromJust ld

-- | Modifica os dados do Jogador no nível atual.
setPlayerData :: GameState -> LocalPlayerData -> Maybe GameState
setPlayerData gs pd
    | isNothing ld = Nothing
    | otherwise = Just $ gs { levelData = Just ((fromJust ld) { playerData = pd }) }
    where
        ld = levelData gs

-- | Pausa o jogo.
pauseGame :: GameState -> Maybe GameState
pauseGame gs
    | isNothing ld = Nothing
    | otherwise = Just $ ((`setSelectedMenu` MID_Paused) . (`setSelectedMenuButton` 0)) gs { state = gss { inGameMenu = True } }
    where
        gss = state gs
        ld = levelData gs

-- | Retoma o jogo.
unpauseGame :: GameState -> Maybe GameState
unpauseGame gs
    | isNothing ld = Nothing
    | otherwise = Just $ gs { state = gss { inGameMenu = False } }
    where
        gss = state gs
        ld = levelData gs

-- | Mata o Jogador no nível atual.
killPlayer :: GameState -> Maybe GameState
killPlayer gs
    | isNothing ld = Nothing
    | isInGameMenu gs = Just gs
    | otherwise = 
        Just $ selectMenu (gs { 
            state = gss { inGameMenu = True }, 
            levelData = Just ((fromJust ld) { killed = True }) 
        }) MID_Killed
    where
        gss = state gs
        ld = levelData gs

-- | Revive o Jogador no nível atual.
revivePlayer :: GameState -> Maybe GameState
revivePlayer gs
    | isNothing ld = Nothing
    | otherwise = Just $ gs { state = gss { inGameMenu = False }, levelData = Just ((fromJust ld) { killed = False }) }
    where
        gss = state gs
        ld = levelData gs

{-
=======================================
        PLAYER DATA SETTERS            
=======================================
-}

-- | Modifica os dados do Jogador no nível atual.
setPlayer :: GameState -> Player -> Maybe GameState
setPlayer gs p
    | isNothing ld = Nothing
    | otherwise = Just $ gs { levelData = Just ((fromJust ld) { game = Game p m }) }
    where
        ld = levelData gs
        (Game _ m) = game $ fromJust ld

-- | Modifica o Offset do Jogador no nível atual.
setPlayerOffset :: GameState -> MapOffset -> Maybe GameState
setPlayerOffset gs o
    | isNothing ld = Nothing
    | otherwise = Just $ gs { levelData = Just ((fromJust ld) { game = Game (Player o v c) m }) }
    where
        ld = levelData gs
        (Game (Player _ v c) m) = game $ fromJust ld

-- | Modifica as coordenadas do Jogador no nível atual.
setPlayerCoords :: GameState -> Coordinates -> Maybe GameState
setPlayerCoords gs c
    | isNothing ld = Nothing
    | otherwise = Just $ gs { levelData = Just ((fromJust ld) { game = Game (Player o v c) m }) }
    where
        ld = levelData gs
        (Game (Player o v _) m) = game $ fromJust ld

-- | Modifica a velocidade do Jogador no nível atual.
setPlayerVelocity :: GameState -> Velocity -> Maybe GameState
setPlayerVelocity gs v
    | isNothing ld = Nothing
    | otherwise = Just $ gs { levelData = Just ((fromJust ld) { game = Game (Player o v c) m }) }
    where
        ld = levelData gs
        (Game (Player o _ c) m) = game $ fromJust ld

-- | Modifica os pontos do Jogador no nível atual.
setPlayerLevelPoints :: GameState -> Int -> Maybe GameState
setPlayerLevelPoints gs pc
    | isNothing ld = Nothing
    | otherwise = Just $ gs { levelData = Just ((fromJust ld) { playerData = pd { points = pc } }) }
    where
        ld = levelData gs
        pd = playerData $ fromJust ld

{-
=======================================
            CHEAT VERIFIERS            
=======================================
-}

-- | Determina se o jogador encontra-se em NoClip.
isNoClip :: GameState -> Bool
isNoClip gs 
    | isNothing ld = False
    | otherwise = Render.Play.Internal.noclip $ flags gs
    where
        ld = levelData gs

-- | Determina se o jogador encontra-se em GodMode.
isGodMode :: GameState -> Bool
isGodMode gs
    | isNothing ld = False
    | otherwise = Render.Play.Internal.godmode $ flags gs
    where
        ld = levelData gs
        