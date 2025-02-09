{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{- 
 Justificação para a desabiliação dos avisos de ambiguidade: a utilização do mesmo nome para diferentes estruturas de
dados é empregada devido á existência de uma estrutura de dados do utilizador para um único nível e uma estrutura de dados
do utilizador de uso global, contendo entradas da estrutura local, e a desâmbiguação das entradas com um prefixo iria poluir
o código com nomes extensos. No entanto, para que o código permaneça compreensível, é pedido ao desenvolvedor que dê nomes
que reflitam a estrutura utilizada ou anotações que permitam resolver a âmbiguidade.
-}

module Render.Play.Internal where

import LI12223
import Util
import Util.Record (setField, getField)
import Util.Logger (LoggerGen)
import GHC.Generics
import Data.Binary
import Data.Data
import Data.Maybe (isNothing, fromJust)
import Data.List (elemIndex, intercalate)
import qualified Data.Set as S
import qualified Data.Map as M
import Locale (Locale (lang), LocaleId (..), getCachedLocale)
import Graphics.Gloss.Interface.IO.Game (Key)
import qualified Graphics.Gloss as G
import Render.Backend.Font

{-
=======================================
                SETTINGS               
=======================================
-}

{- |
 A estrutura @GameState@ define os dados de um jogo.

 == Mapa de Estrutura

 JSON-notation + Haskell (type-notations + comments):  
 
 @
    {
        "flags": { -- Flags que modificam o jogo globalmente.
            "debug"   :: Bool, -- Indica se o jogo foi iniciado no modo debug.
            "godmode" :: Bool, -- Indica se o jogador possuí o GodMode ativado.
            "noclip"  :: Bool  -- Indica se o jogador possuí o NoClip ativado.
        },
        "state": { -- O estado global do jogo.
            "inGame" :: Bool,    -- Indica se o jogador se encontra num jogo.
            "inMenu" :: Bool,    -- Indica se o jogador se encontra num menu.
            "inGameMenu" :: Bool -- Indica se o jogador se encontra num menu dentro de um jogo.
        },
        "levelData": { -- O estado do nível atual.
            "mapType" :: MapType,  -- O tipo do mapa atual.
            "game" :: Jogo,        -- Os dados da grid do 'Mapa' atual.
            "paused" :: Bool,      -- Indica se o jogador possuí o jogo pausado.
            "killed" :: Bool,      -- Indica se o jogador morreu.
            "playerData": {
                "points" :: Integer -- Os pontos obtidos na ronda atual.
            }
        },
        "globalPlayerData": { -- O estado global do jogador.
            "highScore" :: Integer -- A maior pontuação atingida pelo jogador.
        },
        "internal": { -- Cache interna
            "randomGen" :: [Int],                 -- O gerador de números aleatórios do jogo.
            "locale" :: Locale,                   -- O Locale atualmente usado.
            "logger" :: Logger                    -- O Logger atualmente usado.
            "keys" :: S.Set Key,                  -- O Set de teclas atualmente pressionadas.
            "clock" :: Clock,                     -- Os valores dos temporizadores utilizados.
            "textures" :: M.Map String G.Picture, -- A memória de Texturas atualmente carregadas.
            "fonts" :: M.Map String FontData      -- A memória de Fonts atualmente carregadas.
        }
    }
 @

-}

data GameState = GameState {
    flags :: Flags,
    state :: GSState,
    levelData :: Maybe LevelData,
    globalPlayerData :: GlobalPlayerData,
    internal :: InternalData,
    menu :: MenuData
} deriving (Show)

data Flag = Debug | GodMode | NoClip deriving (Eq,Show)
data Flags = Flags {
    debug :: Bool,
    godmode :: Bool,
    noclip :: Bool
} deriving (Generic, Data, Typeable, Show)

data GSState = GSState {
    inGame :: Bool,
    inMenu :: Bool,
    inGameMenu :: Bool
} deriving (Show)

data MapType = REGULAR | CUSTOM deriving (Generic,Eq,Show)
instance Binary MapType

type Velocity = (Float,Float)
type Coordinates = (Float,Float)
type MapOffset = Float

data Map = Map Int MapOffset [(Terreno, [Obstaculo])] deriving (Generic, Read, Eq)
instance Binary Map

#ifdef DEBUG
instance Show Map where
    show (Map w off []) = "Map " ++ show w ++ " " ++ show off ++ " []"
    show (Map w off l) = 
      "Map " ++ show w ++ " " ++ show off ++ " [\n" 
      ++ intercalate ",\n" (
            map (\(t,ol) -> 
                "  (" ++
                padString (show t) ' ' mTL ++ " ["
                ++ if null ol then "]" else
                    padString (
                        intercalate ", " (map (\o -> 
                            case o of {
                                Carro -> "Carro ";
                                _ -> show o;
                            }
                        ) ol)
                    ) ' ' mOL
                    ++ "]"
                ++ ")"
            ) l
        )
      ++ "\n]"
      where
        mTL = maximum (map (\(t,_) -> length (show t)) l)
        mOL = maximum (map (\(_,ol) -> 
                length (
                    intercalate ", " (map (\o -> 
                        case o of {
                            Carro -> "Carro ";
                            _ -> show o;
                        }
                    ) ol)
                )
            ) l)
#else
instance Show Map where
    show (Map w off l) = "Map " ++ show w ++ " " ++ show off ++ " " ++ show l 
#endif

data Player = Player MapOffset Velocity Coordinates deriving (Generic, Show, Read, Eq)
instance Binary Player

data Game = Game Player Map deriving (Generic, Show, Read, Eq)
instance Binary Game
data LevelData = LevelData {
    mapType :: MapType,
    game :: Game,
    paused :: Bool,
    killed :: Bool,
    playerData :: LocalPlayerData
} deriving (Show)

data LocalPlayerData = LocalPlayerData {
    points :: Int
} deriving (Show)

data GlobalPlayerData = GlobalPlayerData {
    highScore :: Integer
} deriving (Generic, Show)
instance Binary GlobalPlayerData

data InternalData = InternalData {
    randomGen :: RndGen,
    locale :: Locale,
    logger :: LoggerGen,
    keys :: S.Set Key,
    clock :: Clock,
    textures :: M.Map String G.Picture,
    fonts :: M.Map String FontData
} deriving (Show)

data Clock = Clock {
    clock1 :: Int,   -- 9 Segundos
    clock2 :: Float, -- Smooth desliza jogo
    clock3 :: Int,   -- 1 Segundo dos Pontos
    clock4 :: Int    -- 1 Segundo dos Cheats
} deriving (Show)

newtype RndGen = RndGen {
    store :: [Int]
}
instance Show RndGen where
    show _ = "[RndGen]"

data MenuId = 
      MID_MainMenu 
    | MID_Options 
    | MID_Save 
        | MID_Save_DelAllConf 
        | MID_Save_Active
    | MID_Paused 
    | MID_Killed 
    deriving (Show, Eq, Ord)
data MenuData = MenuData {
    selected :: ActiveMenu,
    menus :: M.Map MenuId MenuDescriptor,
    stack :: [MenuId]
} deriving (Show)

data ActiveMenu = ActiveMenu {
    selectedMenu :: MenuId,
    selectedButton :: Int
} deriving (Show)

data MenuDescriptor = MenuDescriptor {
    defaultButton :: Int,
    buttons :: [Int]
} deriving (Show)


-- | Muda o estado de uma flag de e para ativada.
toggleFlag :: GameState -> Flag -> GameState
toggleFlag gs f = gs { flags = setField fi fd fl }
    where
        fi = ["debug", "godmode", "noclip"] !! fromJust (elemIndex f [Debug,GodMode,NoClip])
        fl = flags gs
        field = getField fi fl :: Bool
        fd = not field

-- | A função @togglePlay@ alterna entre o menu e o jogo.
togglePlay :: GameState -> GameState
togglePlay gs
    | inGameMenu gss || inMenu gss = gs { state = gss { inGame = True, inGameMenu = False, inMenu = False } } 
    | otherwise = gs { state = gss { inGame = False, inGameMenu = True, inMenu = True } }
    where
        gss = state gs

-- | Obtém os dados do nível do jogo, se o jogo estiver ativado.
getLevelData :: GameState -> Maybe LevelData
getLevelData = levelData

-- | Modifica os dados do nível do jogo.
setLevelData :: GameState -> LevelData -> GameState
setLevelData gs ld = gs { levelData = Just ld }

-- | Elimina os dados do nível do jogo.
resetLevelData :: GameState -> GameState
resetLevelData gs = gs { levelData = Nothing }

-- A funçáo @getHighScore@ obtém o valor do HighScore do Jogador.
getHighScore :: GameState -> Integer
getHighScore gs = highScore (globalPlayerData gs)

-- | Modifica o High Score do jogador para o valor fornecido se for maior que o HS atual. 
-- Caso contrário retorna o GameState atual.
setHighScore :: 
    GameState    -- ^ O valor do GameState atual.
    -> Integer   -- ^ O novo valor do HighScore.
    -> GameState -- ^ O valor do GameState modificado.
setHighScore gs hs
    | chs < hs = gs { globalPlayerData = gpd { highScore = hs } }
    | otherwise = gs
    where
        gpd = globalPlayerData gs
        chs = highScore gpd

-- | Modifica o valor dos pontos que o jogador possuí globalmente.
modifyPoints :: 
    GameState         -- ^ O valor do GameState atual.
    -> Int            -- ^ O número de pontos a adicionar/subtrair do jogador.
    -> GameState      -- ^ O valor do GameState modificado.
modifyPoints gs n 
    | lpcn + n >= 0 = gs { levelData = Just $ ld { playerData = lpd { points = lpcn + n } } }
    | otherwise = error "Attempted to set negative points."
    where
        ld = fromJust (getLevelData gs)
        lpd = playerData ld
        lpcn = points (lpd :: LocalPlayerData)

-- | Obtém um número aleatório a partir do gerador presente na cache do GameState atual.
getRandom :: GameState -> (Int,GameState)
getRandom gs = (head (store $ randomGen igs), gs { internal = igs { randomGen = RndGen (tail (store $ randomGen igs)) } })
    where
        igs = internal gs

-- | A função @getLocale@ obtém os dados do 'Locale' atualmente carregado.
getLocale :: GameState -> Locale
getLocale = locale . internal

-- | A função @getLocaleId@ obtém o identificador do 'Locale' atualmente carregado.
getLocaleId :: GameState -> LocaleId
getLocaleId gs = let ids = M.fromList [("en_US", EN_US),("pt_PT", PT_PT)] in fromJust $ M.lookup (lang (getLocale gs)) ids

-- | Muda o Locale do GameState.
changeLocale :: GameState -> LocaleId -> IO GameState
changeLocale gs l = do
    locale <- getCachedLocale l
    return $ gs { internal = (internal gs) { locale = locale } }

-- | Adiciona uma Key á lista de keypresses.
addKey :: GameState -> Key -> GameState
addKey gs k = gs { internal = igs { keys = S.insert k (keys igs) } }
    where
        igs = internal gs

-- | Remove uma Key da lista de keypresses.
removeKey :: GameState -> Key -> GameState
removeKey gs k = gs { internal = igs { keys = S.delete k (keys igs) } }
    where
        igs = internal gs

-- | Verifica se uma Key existe na lista de keypresses.
hasKey :: GameState -> Key -> Bool
hasKey gs k = S.member k kigs
    where
        igs = internal gs
        kigs = keys igs

getClock :: GameState -> Clock
getClock = clock . internal

setClock :: GameState -> Clock -> GameState
setClock gs c = let intr = internal gs in gs { internal = intr { clock = c } }

-- Clock 1
getClock1Value :: GameState -> Int
getClock1Value gs = clock1 (clock (internal gs))

setClock1Value :: GameState -> Int -> GameState
setClock1Value gs cv = let {intr = internal gs; cgs = clock intr} in gs { internal = intr { clock = cgs {clock1 = cv} } }

-- Clock 2
getClock2Value :: GameState -> Float
getClock2Value gs = clock2 (clock (internal gs))

setClock2Value :: GameState -> Float -> GameState
setClock2Value gs cv = let {intr = internal gs; cgs = clock intr} in gs { internal = intr { clock = cgs {clock2 = cv} } }

-- Clock 3
getClock3Value :: GameState -> Int
getClock3Value gs = clock3 (clock (internal gs))

setClock3Value :: GameState -> Int -> GameState
setClock3Value gs cv = let {intr = internal gs; cgs = clock intr} in gs { internal = intr { clock = cgs {clock3 = cv} } }

-- Clock 4
getClock4Value :: GameState -> Int
getClock4Value gs = clock4 (clock (internal gs))

setClock4Value :: GameState -> Int -> GameState
setClock4Value gs cv = let {intr = internal gs; cgs = clock intr} in gs { internal = intr { clock = cgs {clock4 = cv} } }



-- | A função @getTextures@ obtém as Texturas atualmente carregadas.
getTextures :: GameState -> M.Map String G.Picture
getTextures = textures . internal

-- | A função @addTextures@ adiciona um mapa de Texturas á memória.
addTextures :: GameState -> M.Map String G.Picture -> GameState
addTextures gs tl = gs { internal = itd { textures = M.union (textures itd) tl } } --textures itd ++ tl
    where
        itd = internal gs

-- | A função @getFonts@ obtém as Fonts atualmente carregadas.
getFonts :: GameState -> M.Map String FontData
getFonts = fonts . internal

-- | A função @addFonts@ adiciona um mapa de Fonts á memória.
addFonts :: GameState -> M.Map String FontData -> GameState
addFonts gs tl = gs { internal = itd { fonts = M.union (fonts itd) tl } }
    where
        itd = internal gs

-- | A função @getSelectedMenu@ obtém o Menu selecionado.
getSelectedMenu :: GameState -> MenuId
getSelectedMenu = selectedMenu . selected . menu

-- | A função @setSelectedMenu@ modifica o Menu selecionado.
setSelectedMenu :: GameState -> MenuId -> GameState
setSelectedMenu gs i
    | M.member i ms = gs { menu = m { selected = sm { selectedMenu = i } } }
    | otherwise = error "Menu Index Out Of Bounds."
    where
        m@(MenuData sm ms _) = menu gs

-- | A função @getSelectedMenuButton@ obtém o botão selecionado do Menu selecionado.
getSelectedMenuButton :: GameState -> Int
getSelectedMenuButton gs = (selectedButton . selected . menu) gs
        where
            m = getSelectedMenu gs

-- | A função @setSelectedMenuButton@ modifica o botão selecionado do Menu selecionado.
setSelectedMenuButton :: GameState -> Int -> GameState
setSelectedMenuButton gs i
    | elem i btns = gs { menu = m { selected = sm { selectedButton = i } } }
    | otherwise = gs
        where
            m@(MenuData sm ms _) = menu gs
            btns = buttons (getSelectedMenuData gs)

-- | A função @setSelectedMenuData@ obtém o valor do Menu selecionado.
getSelectedMenuData :: GameState -> MenuDescriptor
getSelectedMenuData gs = fromJust $ M.lookup ((selectedMenu . selected . menu) gs) ((menus . menu) gs)

-- | A função @setSelectedMenuData@ modifica o valor do Menu selecionado.
setSelectedMenuData :: GameState -> MenuDescriptor -> GameState
setSelectedMenuData gs md = gs { menu = m { menus = M.update (const $ Just md) ((selectedMenu . selected . menu) gs) ((menus . menu) gs) } }
    where
        m = menu gs

-- | A função @shiftNSelectedMenuButton@ seleciona o menu colocado a @n@ posições do botão atualmente selecionado @i@.
-- Caso @n + i@ exceda o tamanho da lista de botões,@l@ a posição será @n + i - length l@
shiftNSelectedMenuButton :: GameState -> Int -> GameState
shiftNSelectedMenuButton gs n = setSelectedMenuButton gs (buttons md !! (head . shiftN (-n)) il)
    where
        md = getSelectedMenuData gs
        mi = fromJust $ elemIndex (getSelectedMenuButton gs) (buttons md)
        _il = [0..length (buttons md) - 1]
        il = drop mi _il ++ take mi _il

-- | A função @appendToMenuStack@ adiciona um menu ao histórico de menus.
appendToMenuStack :: GameState -> MenuId -> GameState
appendToMenuStack gs i
    | M.member i ms = gs { menu = m { stack = st ++ [i] } }
    | otherwise = error ("Menu not defined: " ++ show i)
    where
        m@(MenuData _ ms st) = menu gs

-- | A função @backMenuStack@ remove o último menu do histórico de menus.
popFromMenuStack :: GameState -> GameState
popFromMenuStack gs = ngs
    where 
        m@(MenuData s mns st) = menu gs
        md = fromJust $ M.lookup (last st) mns
        ngs = gs { menu = m { stack = init st } }

-- | A função @getMenuStack@ obtém o valor do histórico de menus.
getMenuStack :: GameState -> [MenuId]
getMenuStack = stack . menu

-- | A função @setMenuStack@ muda o valor do histórico de menus.
setMenuStack :: GameState -> [MenuId] -> GameState
setMenuStack gs s = gs { menu = m { stack = s } }
    where
        m = menu gs

-- | A função @backMenuStack@ seleciona o menu anterior no histórico de menus.
backMenuStack :: GameState -> GameState
backMenuStack gs
    | null ns = gs
    | otherwise = setSelectedMenu ngs (last ns)
    where
        ngs = popFromMenuStack gs
        ns = (stack . menu) ngs

-- | A função @pushMenuStack@ adiciona um menu ao histórico de menus e seleciona-o.
pushMenuStack :: GameState -> MenuId -> GameState
pushMenuStack gs i = setSelectedMenu ngs i
    where
        ngs = appendToMenuStack gs i

-- | A função @selectaMenu@ seleciona um menu.
selectMenu :: GameState -> MenuId -> GameState
selectMenu gs mi = do
    let ngs = pushMenuStack gs mi
    let ngs2 = setSelectedMenuButton ngs (defaultButton $ getSelectedMenuData ngs)

    ngs2

-- | A função @desselectMenu@ seleciona o menu anteriormente selectionado.
desselectMenu gs = do
    let ngs = backMenuStack gs
    let ngs2 = setSelectedMenuButton ngs (defaultButton $ getSelectedMenuData ngs)

    ngs2