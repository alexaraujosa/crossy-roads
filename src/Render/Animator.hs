module Render.Animator where

import Graphics.Gloss.Interface.IO.Game
import Render.Play
import Render.Common
import Save
import Render.World.Level
import System.Exit
import Locale
import Data.Maybe
import Util
import Tarefa3_v2 hiding (fps)
import qualified Tarefa3_v2 as T3_2
import Tarefa5_v2
import Render.World.Save
import Tarefa2_v2

fps = T3_2.fps

inputReact :: Event -> (GameState, CacheData) -> IO (GameState, CacheData)
inputReact (EventKey key Down _ _) (gs,cd) 
    -- Menu Principal
    | isInMenu gs && getSelectedMenu gs == MID_MainMenu = case key of { 
        (SpecialKey KeyDown) -> return $ (shiftNSelectedMenuButton gs 1,cd);
        (SpecialKey KeyUp) -> return $ (shiftNSelectedMenuButton gs (-1),cd);
        (SpecialKey KeyEnter) -> case getSelectedMenuButton gs of {
            0 -> return $ (
                togglePlay (setMenuStack gs [MID_MainMenu]) {flags = Flags {debug = False, godmode = False, noclip = False}},
                cd
            );
            1 -> return $ (selectMenu gs MID_Save,cd);
            2 -> return $ (selectMenu gs MID_Options,cd);
            3 -> exitSuccess;
        };
        _ -> return (gs,cd)
    }

    -- Menu de Opções
    | isInMenu gs && getSelectedMenu gs == MID_Options = case key of { 
        (SpecialKey KeyDown) -> return $ (shiftNSelectedMenuButton gs 1,cd);
        (SpecialKey KeyUp) -> return $ (shiftNSelectedMenuButton gs (-1),cd);
        (SpecialKey KeyEnter) -> case getSelectedMenuButton gs of {
            0 -> return $ (desselectMenu gs,cd);
            1 -> (do
                let l = getLocaleId gs
                nl <- if l == EN_US then changeLocale gs PT_PT else changeLocale gs EN_US
                return (nl,cd)
                );
            _ -> return $ (gs,cd);
        };
        (SpecialKey KeyEsc) -> return $ (desselectMenu gs,cd);
        _ -> return $ (gs,cd)
    }

    -- Menu do Deadscreen
    | isInMenu gs && getSelectedMenu gs == MID_Killed = return $ case key of { 
        (SpecialKey KeyDown) -> (shiftNSelectedMenuButton gs 1,cd);
        (SpecialKey KeyUp) -> (shiftNSelectedMenuButton gs (-1),cd);
        (SpecialKey KeyEnter) -> case getSelectedMenuButton gs of {
            0 -> (do 
                    let ngs = (fromJust (
                            revivePlayer (posicionarJogadorInicio $ geraMapaInicial $ setLevelData scgs defaultLevelData)
                            ) `setMenuStack` [MID_MainMenu])
                    let (Map w _ ol) = fromJust $ getGameMap ngs

                    let ngs2 = fromJust $ setGameMap ngs (Map w 0 ol)
                    
                    let itd = internal ngs2
                    let cdt = clock itd
                    let ngs3 = ngs {
                            internal = itd {
                                    clock = cdt {
                                        clock1 = 0,
                                        clock2 = 0,
                                        clock3 = 0,
                                        clock4 = 0
                                    }
                                }
                            }

                    (ngs3,cd)
                );
            1 -> (selectMenu gs MID_Options,cd);
            2 -> (resetLevelData $ desselectMenu gs,cd);
        };
        (SpecialKey KeyEsc) -> (resetLevelData $ desselectMenu gs,cd);
        _ -> (gs,cd)
    }

    -- Menu de Pausa
    | isInMenu gs && getSelectedMenu gs == MID_Paused = return $ case key of { 
        (SpecialKey KeyDown) -> (shiftNSelectedMenuButton gs 1,cd);
        (SpecialKey KeyUp) -> (shiftNSelectedMenuButton gs (-1),cd);
        (SpecialKey KeyEnter) -> case getSelectedMenuButton gs of {
            0 -> (fromJust $ unpauseGame gs,cd);                   -- Unpause
            1 -> (selectMenu gs MID_Save,cd { allowSave = True }); -- Save
            2 -> (selectMenu gs MID_Options,cd);                   -- Options
            3 -> (do                                               -- Exit 
                    let ngs = posicionarJogadorInicio $ geraMapaInicial $ setLevelData scgs defaultLevelData
                    let (Map w _ ol) = fromJust $ getGameMap ngs

                    let ngs2 = fromJust $ setGameMap ngs (Map w 0 ol)
                    
                    let itd = internal ngs2
                    let cdt = clock itd
                    let ngs3 = ngs {
                            internal = itd {
                                    clock = cdt {
                                        clock1 = 0,
                                        clock2 = 0,
                                        clock3 = 0,
                                        clock4 = 0
                                    }
                                }
                            }
                    (desselectMenu ngs3,cd)
                );
        };
        (SpecialKey KeyEsc) -> (fromJust $ unpauseGame gs,cd);
        _ -> (gs,cd)
    }

    -- Menu de Saves
    | isInMenu gs && getSelectedMenu gs == MID_Save = case key of { 
        (SpecialKey KeyDown) -> return $ shiftSaveButtons (gs,cd) 1;
        (SpecialKey KeyUp) -> return $ shiftSaveButtons (gs,cd) (-1);
        (SpecialKey KeyEnter) -> case getSelectedMenuButton gs of {
            sel
                | elem sel [0,1,2,3] && allowSave cd -> (do
                    let isSaveActive = isJust $ saves cd !! sel

                    if isSaveActive then return $ (
                            selectMenu gs MID_Save_Active, 
                            cd { lastMenuSnapshot = (MID_Save,sel,[]), menuAditionalText = [show (sel)] }
                        )
                    else do
                        sd <- toSaveData gs
                        writeSave sd (saveNames cd !! sel)
                        nsd <- loadSaves

                        return $ (gs,cd { saves = nsd })
                    )
                | elem sel [0,1,2,3] -> if isJust $ saves cd !! sel then return $ (
                        selectMenu gs MID_Save_Active, 
                        cd { lastMenuSnapshot = (MID_Save,sel,[]), menuAditionalText = [show (sel)] }
                    )
                    else return $ shiftSaveButtons (gs,cd) 1
            ;
            4 -> return $ (
                selectMenu gs MID_Save_DelAllConf, 
                cd { lastMenuSnapshot = (MID_Save,4,[]), menuAditionalText = ["0"] }
                );
            5 -> return $ (desselectMenu gs,cd { allowSave = False });
        };
        (SpecialKey KeyEsc) -> return $ (desselectMenu gs,cd { allowSave = False });
        _ -> return $ (gs,cd)
    }
    
    -- Menu de Save Individual
    | isInMenu gs && getSelectedMenu gs == MID_Save_Active = do
        let saveData = saves cd
        let sfs = map isJust saveData
        let ocsd = saves cd
        let mat = menuAditionalText cd
        
        case key of { 
            (SpecialKey KeyDown) -> let osgs = shiftNSelectedMenuButton gs 1 in 
                if (getSelectedMenuButton osgs == 1 && not (isInGameMenu gs)) then return $ (shiftNSelectedMenuButton osgs 1, cd)
                else return $ (osgs, cd)
            ;
            (SpecialKey KeyUp) -> let osgs = shiftNSelectedMenuButton gs (-1) in 
                if (getSelectedMenuButton osgs == 1 && not (isInGameMenu gs)) then return $ (shiftNSelectedMenuButton osgs (-1), cd)
                else return $ (osgs, cd)
            ;
            (SpecialKey KeyEnter) -> 
                let sel = (read . head) mat in
                case getSelectedMenuButton gs of
                    0 ->     -- Load Save
                        if sfs !! (sel) then do
                            let sid = (sel)
                            nsd <- loadSave (saveNames cd !! sid)
                            let ncd = cd { saves = take sid ocsd ++ [Just nsd] ++ drop (sid + 1) ocsd }
                                        
                            sdgs <- fromSaveData nsd
                            let ngs = togglePlay $ setMenuStack gs { 
                                    levelData = levelData sdgs, 
                                    globalPlayerData = globalPlayerData sdgs 
                                } [MID_MainMenu,MID_Save] 

                            return $ (ngs,ncd)
                        else return (gs,cd)
                    ;
                    1 -> (do -- Save Save
                            let osd = saves cd !! sel
                            gsd <- toSaveData gs
                            let sd = if isNothing osd then gsd else 
                                    let rosd = fromJust osd in
                                    gsd { 
                                        header = (header gsd) { 
                                            cdate = (cdate . header) rosd,
                                            times_saved = (times_saved . header) rosd + 1
                                        } 
                                    }

                            writeSave sd (saveNames cd !! sel)

                            return (gs,cd { saves = replaceAt (saves cd) sel (Just sd) })
                        );
                    2 -> return $ ( -- Delete Save
                            selectMenu gs MID_Save_DelAllConf, 
                            cd { 
                                lastMenuSnapshot = (MID_Save,(snd3 . lastMenuSnapshot) cd,mat), 
                                menuAditionalText = ["1",show (sel)] 
                            }
                        );
                    3 -> return (desselectMenu gs,cd); -- Exit
            ;
            (SpecialKey KeyEsc) -> return (desselectMenu gs,cd);
            _ -> return (gs,cd)
        }

    -- Confirmação de eliminação de saves
    | isInMenu gs && getSelectedMenu gs == MID_Save_DelAllConf = do
        case key of { 
            (SpecialKey KeyDown) -> return $ (shiftNSelectedMenuButton gs 1,cd);
            (SpecialKey KeyUp) -> return $ (shiftNSelectedMenuButton gs (-1),cd);
            (SpecialKey KeyEnter) -> 
                let mat = menuAditionalText cd in case getSelectedMenuButton gs of {
                0 -> return $ restoreMenuSnapshot (gs,cd);
                1 -> if mat !! 0 == "0" then (do
                        deleteSaves

                        sd <- loadSaves
                        return $ restoreMenuSnapshot (gs, cd { saves = sd });
                ) else (do
                    deleteSave (read $ mat !! 1 :: Int)
                    sd <- loadSaves

                    return $ restoreMenuSnapshot (gs, cd { saves = sd })
                ) ;
            };
            (SpecialKey KeyEsc) -> return $ restoreMenuSnapshot (gs,cd);
            _ -> return $ (gs,cd)
        }

    -- Dentro de um jogo
    | isInGame gs && not (isPaused gs) = return $ case key of {
        (SpecialKey KeyDown) -> (addKey gs (SpecialKey KeyDown),cd);
        (SpecialKey KeyUp) -> (addKey gs (SpecialKey KeyUp),cd);
        (SpecialKey KeyLeft) -> (addKey gs (SpecialKey KeyLeft),cd);
        (SpecialKey KeyRight) -> (addKey gs (SpecialKey KeyRight),cd);
        (SpecialKey KeyEsc) -> (fromJust $ pauseGame (selectMenu gs MID_Paused),cd);
        (Char c) -> verifyKeySequence (gs,addToKeySequence cd c);
        _ -> (gs,cd)
    }

    | otherwise = return (gs,cd)
    where
    scgs = ((flip setClock1Value 0) . (flip setClock2Value 0) . (flip setClock3Value 0) . (flip setClock4Value 0)) gs
    shiftSaveButtons (gs,cd) n 
        | n > 0 && elem sel [5,0,1,2] && not (allowSave cd) = (rotUntil gs n sfs,cd)
        | n < 0 && elem sel [1,2,3,4] && not (allowSave cd) = (rotUntil gs n sfs,cd)
        | otherwise = (shiftNSelectedMenuButton gs n,cd)
        where
            sel = getSelectedMenuButton gs
            sfs = map isJust (saves cd)
            rot = shiftNSelectedMenuButton gs n
            rotUntil gs n sfs
                | getSelectedMenuButton rot > 3 || sfs !! getSelectedMenuButton rot = rot
                | otherwise = rotUntil rot n sfs
                where
                    rot = shiftNSelectedMenuButton gs n                 
inputReact _ gs = return gs
     
timeReact :: Float -> (GameState,CacheData) -> IO (GameState,CacheData)
timeReact n (gs,cd)
    | isDead (jogoTerminou gs) = return $ (fromJust $ killPlayer $ setHighScore gs (fromIntegral $ fromJust $ getPlayerLevelPoints gs),cd)
    | isPaused gs || isInMenu gs || isInGameMenu gs = return (ngs,cd)
    | getClock4Value gs > round fps = return $ (setClock4Value gs 0,cd)
    | getClock3Value gs >= round fps = case fromJust (getPlayerLevelPoints gs) of {
        points 
            | points > 3052 -> 
                return $ (setClock3Value (fromJust $ setPlayerLevelPoints gs (fromJust (getPlayerLevelPoints gs) + 43)) 0,cd)
            | points > 2026 -> 
                return $ (setClock3Value (fromJust $ setPlayerLevelPoints gs (fromJust (getPlayerLevelPoints gs) + 38)) 0,cd)
            | points > 1297 -> 
                return $ (setClock3Value (fromJust $ setPlayerLevelPoints gs (fromJust (getPlayerLevelPoints gs) + 27)) 0,cd)
            | points > 783 -> 
                return $ (setClock3Value (fromJust $ setPlayerLevelPoints gs (fromJust (getPlayerLevelPoints gs) + 19)) 0,cd)
            | points > 405 -> 
                return $ (setClock3Value (fromJust $ setPlayerLevelPoints gs (fromJust (getPlayerLevelPoints gs) + 14)) 0,cd)
            | points > 162 -> 
                return $ (setClock3Value (fromJust $ setPlayerLevelPoints gs (fromJust (getPlayerLevelPoints gs) + 9)) 0,cd)
            | otherwise -> 
                return $ (setClock3Value (fromJust $ setPlayerLevelPoints gs (fromJust (getPlayerLevelPoints gs) + 6)) 0,cd)

    }

    
    | getClock1Value gs >= round (9*fps) = do
        case fromJust (getPlayerLevelPoints gs) of { 
            points | points > 3052 -> do nextTick 3000 4
                   | points > 2026 -> do nextTick 2500 3
                   | points > 1297 -> do nextTick 2000 2
                   | points > 783 -> do nextTick 1500 2
                   | points > 405 -> do nextTick 1000 2
                   | points > 162 -> do nextTick 750 1
                   | otherwise -> do nextTick 500 1
            }

    | otherwise = do
        let ngs = animaMapa (animaJogador gs)
        let itd = internal ngs
        let cdt = clock itd

        return $ (
            ngs {
                internal = itd {
                    clock = cgs {
                        clock1 = getClock1Value gs + 1,
                        clock2 = getClock2Value gs + (yy/ (9*fps)),
                        clock3 = getClock3Value gs + 1,
                        clock4 = getClock4Value gs + 1
                    }
                }
            },
            cd
            )
    
    where
        ngs = gs
        (r,ngs2) = getRandom gs
        itd = internal gs
        cgs = clock itd
        (_,yy) = getTileSize mapa
        mapa = fromJust $ getGameMap gs

        makeNGS n n2 = 
            fromJust $ setGame ngs2 (deslizaGame (withinBoundsWA 0 n r) n2 (fromJust $ getGame $ animaMapa $ animaJogador gs))
        nextTick n n2 = do
            let ngs = makeNGS n n2
            let itd = internal ngs
            let cdt = clock itd

            return $ (ngs {internal = itd {clock = cgs {clock1 = 0, clock2 = 0}}},cd)

geraMapaInicial :: GameState -> GameState 
geraMapaInicial gs 
    | length l < 8 = geraMapaInicial $ fromJust $ setGameMap ngs (estendeMapa (fromJust $ getGameMap gs) (withinBoundsWA 0 100 r) 1) -- withinBoundsWA 0 500 r, 500 para serem as velocidades moderadas e nao numeros estapafurdicos, tipo 40
    | otherwise = gs 
    where 
        (r,ngs) = getRandom gs
        (Map w off l) = fromJust $ getGameMap gs

posicionarJogadorInicio :: GameState -> GameState
posicionarJogadorInicio gs = fromJust $ setPlayerCoords gs (fromIntegral $ div w 2, fromIntegral (length l) - 1)
    where (Map w off l) = fromJust $ getGameMap gs