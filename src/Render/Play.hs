{- |
Module      : Render.Play
Description : Módulo de interface Jogador-Jogo.
Copyright   : Alex Araújo de Sá <a104257@alunos.uminho.pt>
              Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de interface Jogador-Jogo. Permite a interação do Jogador com o Jogo e vice-versa. Contém funções que mudam o estado do
Jogo mediante a interação do Jogador.
-}

{-# LANGUAGE DuplicateRecordFields #-}

module Render.Play (
    -- * GameState
    -- ** Estruturas
    --- *** Records
    GameState(..), 
    Flags(..), 
    GSState(..), 
    LevelData(..), LocalPlayerData(..), Map(..), Game(..),
    GlobalPlayerData(..), 
    InternalData(..),
    -- *** Sinalizadores
    Flag(..), 
    MapType(..), 
    Player(..),
    Velocity,
    Coordinates,
    MapOffset,
    MenuId(..),
    Clock(..),

    -- ** Funções
    toggleFlag, 
    togglePlay,
    getLevelData, setLevelData, resetLevelData,
    getHighScore, setHighScore,
    modifyPoints, 
    getRandom,
    getLocale, getLocaleId, changeLocale,
    addKey, removeKey, hasKey,
    getClock, setClock, getClock1Value, setClock1Value, getClock2Value, setClock2Value, getClock3Value, setClock3Value, getClock4Value, setClock4Value,
    addTextures, getTextures,
    addFonts, getFonts,
    getSelectedMenu, setSelectedMenu,
    getSelectedMenuButton, setSelectedMenuButton,
    getSelectedMenuData, setSelectedMenuData,
    shiftNSelectedMenuButton,
    getMenuStack, setMenuStack, backMenuStack, popFromMenuStack,
    selectMenu, desselectMenu,
    isNoClip, isGodMode,

    -- ** Constantes
    -- powerUpEffectList,
    defaultGameState,
    defaultLevelData
) where

import Render.Play.Internal
import Render.World
import qualified Graphics.Gloss as G
import Locale (en_US)
import Util (getDate)
import System.Random
import Util.Logger ( defaultLogData, loggerMixed )
import qualified Data.Set as S
import qualified Data.Map as M
import Render.World.Level

-- | Valores iniciais do LevelData.
defaultLevelData :: LevelData
defaultLevelData = LevelData {
    mapType = REGULAR,
    game = Game (Player 0 (0,0) (2,5)) (defaultWorld 9 1),
    paused = False,
    killed = False,
    playerData = LocalPlayerData {
        points = 0
    }
}

-- | Valores iniciais do GameState.
defaultGameState :: Int -> IO GameState
defaultGameState s = do
    defaultLocale <- en_US
    (d,m,y) <- getDate

    return $ GameState {
    flags = Flags {
        debug = False,
        godmode = False,
        noclip = False
    },
    state = GSState {
        inGame = False,
        inMenu = True,
        inGameMenu = False
    },
    levelData = Nothing,
    globalPlayerData = GlobalPlayerData {
        highScore = 0
    },
    internal = InternalData {
        randomGen = RndGen (randoms (mkStdGen s)),
        locale = defaultLocale,
        logger = loggerMixed defaultLogData (show d ++ "_" ++ show m ++ "_" ++ show y),
        keys = S.empty,
        clock = Clock {
            clock1 = 0, -- ^ Clock utilizado para o deslizaGame (9 Segundos)
            clock2 = 0, -- ^ Clock com valores para o Translate suavizado do deslizaGame
            clock3 = 0, -- ^ Clock utilizado para o sistema de pontuação (1 Segundo)
            clock4 = 0  -- ^ Clock utilizado para o Konami Code (1 Segundo)
        },
        textures = M.empty,
        fonts = M.empty
    },
    menu = MenuData {
        selected = ActiveMenu MID_MainMenu 0,
        menus = M.fromList [
            (MID_MainMenu, MenuDescriptor 0 [0,1,2,3]),
            (MID_Options, MenuDescriptor 0 [0,1,2]),
            (MID_Save, MenuDescriptor 0 [0,1,2,3,4,5]),
            (MID_Save_DelAllConf, MenuDescriptor 0 [0,1]),
            (MID_Save_Active, MenuDescriptor 0 [0,1,2,3]),
            (MID_Paused, MenuDescriptor 0 [0,1,2,3]),
            (MID_Killed, MenuDescriptor 0 [0,1,2])
        ],
        stack = [MID_MainMenu]
    }
}