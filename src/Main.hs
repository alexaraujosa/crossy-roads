module Main where

import LI12223
import Tarefa1_2022li1g003
import Tarefa2_2022li1g003
import Tarefa3_2022li1g003
import Tarefa4_2022li1g003
import Render.Play
import qualified Render.Play as RP
import qualified SMTOffsets as SMTO
import qualified Data.Map as M
import Render.Common
import qualified Graphics.Gloss.Interface.IO.Game as G
import Render.Backend.Font
import System.Directory
import System.FilePath
import Util
import Render.Backend.Atlas
import Render.Animator
import Render.World.Save
import Render

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    time <- getTimeSA

    -- Base GameState
    ogs <- RP.defaultGameState (fromIntegral time)

    -- Textures
    textureAtlas <- 
        loadAtlas (cwd ++ [pathSeparator] ++ "kfcrun" ++ [pathSeparator] ++ "textures" ++ [pathSeparator] ++ "atlas")
    let offsets = SMTO.offsets
    let secs = parseAtlasSector textureAtlas (atlas offsets)
    let tgs = addTextures ogs secs

    -- Fonts
    chopsic <- makeFontChopsic
    let fgs = addFonts tgs (M.fromList [("chopsic", chopsic)])

    -- Load LevelData
    let lgs = fgs { state = (state ogs) { inGame=False, inGameMenu=False }, levelData = Just defaultLevelData }
    let ngs = geraMapaInicial lgs
    let ngs2 = posicionarJogadorInicio ngs
        
    -- Load cache
    let dc = defaultCacheData
    sd <- loadSaves
    let scd = dc { saves = sd }

    -- Set menu
    let mgs = selectMenu ngs2 MID_MainMenu

    -- Finalize and initialize gloss
    let gs = mgs
    let cd = scd

    G.playIO mainDisplay
        borderColor
        (round fps)
        (gs,cd)
        render
        inputReact
        timeReact
 

makeFontChopsic = do
    cwd <- getCurrentDirectory
    (FontData f s _) <- makeFont (cwd ++ [pathSeparator] ++ "kfcrun/textures/font/chopsic-unicode") (32,32)
    let oot = replicate 255 (-6)
    let ot = replaceAtIndexes oot [
            (32,-14),
            (33,-20),
            (34,-9),
            (39,-20),
            (40,-18),
            (41,-18),
            (42,-10),
            (43,-10),
            (44,-20),
            (45,-5),
            (46,-20),
            (47,-5),
            (49,-20),
            (50,-4),
            (51,-4),
            (57,-4),
            (58,-20),
            (59,-20),
            (61,-12),
            (72,-3),
            (73,-20),
            (83,-4),
            (87,4),
            (104,-3),
            (105,-20),
            (115,-4),
            (119,4),
            (205,-20),
            (237,-20)
            ]
    
    return (FontData f s ot)
