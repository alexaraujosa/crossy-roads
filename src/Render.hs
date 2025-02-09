{- |
Module      : Render
Description : Módulo principal de Renderização
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo principal de Renderização da componente gráfica do Projeto.
-}

module Render where
import Render.Play
import qualified Render.Play as RP
import qualified Data.Map as M
import Graphics.Gloss
import LI12223
import Render.Backend.Font
import Render.Common
import Save
import Render.World.Level
import Graphics.Gloss.Interface.Environment
import Data.Maybe
import Tarefa3_v2
import Locale
import Util
import Text.Printf
import Data.List

borderColor = makeColorI 116 116 116 255

mainDisplay :: Display
mainDisplay = InWindow "KFC Run" displayDimension (800,0)

render :: (GameState, CacheData) -> IO Picture
render (gs,cd) 
    | isInMenu gs && (not (isInGameMenu gs) || getSelectedMenu gs == MID_MainMenu) = do 
        (wx,wy) <- getScreenSize
        let (rwx,rwy) = (fromIntegral (wx - ww) / 4, fromIntegral wy / 4)
        let borders = Pictures [
                Translate (-(fromIntegral ww / 2) - rwx) 0 $ Scale ((rwx * 2) / 300) 1 $ sidebar,
                Translate ((fromIntegral ww / 2) + rwx) 0 $ Scale ((rwx * 2) / 300) 1 $ sidebar
                ]

        mainmenu <- drawMenus (gs,cd)
        return $ Pictures [mainmenu, borders]
    | isInGame gs = do
        (wx,wy) <- getScreenSize
        let (rwx,rwy) = (fromIntegral (wx - ww) / 4, fromIntegral wy / 4)
        let borders = Pictures [
                Translate (-(fromIntegral ww / 2) - rwx) 0 $ Scale ((rwx * 2) / 300) 1 $ sidebar,
                Translate ((fromIntegral ww / 2) + rwx) 0 $ Scale ((rwx * 2) / 300) 1 $ sidebar
                ]

        gm <- drawGame (gs,cd)

        return $ Pictures [gm, borders]
    | otherwise = do return Blank
    where
        g = RP.game $ fromJust $ getLevelData gs
        (ww,wh) = displayDimension
        txts = getTextures gs
        sidebar = fromJust $ M.lookup "sidebars" txts        


drawMenus :: (GameState, CacheData) -> IO Picture
drawMenus (gs,cd) 
    | isInMenu gs && getSelectedMenu gs == MID_MainMenu = do
        let locale = getLocale gs
        let txts = getTextures gs
        let fonts = getFonts gs
        
        let opcoesp = fromJust $ M.lookup "optionsmenup" txts
        let botao = fromJust $ M.lookup "botao" txts
        let botao2 = fromJust $ M.lookup "botao2" txts
        let sel = getSelectedMenuButton gs

        let chopsic = fromJust $ M.lookup "chopsic" fonts

        let score = drawScore gs (Just (fromIntegral $ getHighScore gs))

        return $ Pictures [
            opcoesp,
            Translate 7 80     $ if sel == 0 then botao2 else botao,
            Translate 7 80     $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_play"),

            Translate 7 (-50)  $ if sel == 1 then botao2 else botao,
            Translate 7 (-50)  $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_save"),

            Translate 7 (-180) $ if sel == 2 then botao2 else botao,
            Translate 7 (-180) $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_options"),
            
            Translate 7 (-310) $ if sel == 3 then botao2 else botao,
            Translate 7 (-310) $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_exit"),
            
            score
            ]
    | isInMenu gs && getSelectedMenu gs == MID_Options = do
        let locale = getLocale gs
        let txts = getTextures gs
        let fonts = getFonts gs
        
        let opcoesp = fromJust $ M.lookup "optionsmenup" txts
        let botao = fromJust $ M.lookup "botao" txts
        let botao2 = fromJust $ M.lookup "botao2" txts
        let sel = getSelectedMenuButton gs

        let chopsic = fromJust $ M.lookup "chopsic" fonts

        return $ Pictures [
            opcoesp,
            Translate 7 55     $ if sel == 0 then botao2 else botao,
            Translate 3 55     $ drawLimitedText chopsic (1,1) (140,32) (getLocalizedStringP locale "menu_resume"),

            Translate 7 (-65) $ if sel == 1 then botao2 else botao,

            if getLocaleId gs == PT_PT then
                Translate 10 (-65) $ drawLimitedText chopsic (1,1) (140,32) $ getLocalizedStringP locale "menu_options_english"
            else
                Translate 0 (-65) $ drawLimitedText chopsic (1,1) (140,32) $ getLocalizedStringP locale "menu_options_portugues"
            ]
    | isInMenu gs && getSelectedMenu gs == MID_Killed = do
        let locale = getLocale gs
        let txts = getTextures gs
        let fonts = getFonts gs

        let chopsic = fromJust $ M.lookup "chopsic" fonts
        
        let morte = fromJust $ M.lookup "deadmenu" txts
        let botao = fromJust $ M.lookup "botao" txts
        let botao2 = fromJust $ M.lookup "botao2" txts
        let sel = getSelectedMenuButton gs

        return $ Pictures [
            morte,
            Translate 7 40     $ if sel == 0 then botao2 else botao,
            Translate 0 40     $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_respawn"),
            
            Translate 7 (-110) $ if sel == 1 then botao2 else botao,
            Translate 2 (-110) $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_options"),

            Translate 7 (-260) $ if sel == 2 then botao2 else botao,
            Translate 0 (-260) $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_exit")
            ]
    | isInMenu gs && getSelectedMenu gs == MID_Paused = do
        let locale = getLocale gs
        let txts = getTextures gs
        let fonts = getFonts gs

        let chopsic = fromJust $ M.lookup "chopsic" fonts

        let pausa = fromJust $ M.lookup "pausemenu" txts
        let botao = fromJust $ M.lookup "botao" txts
        let botao2 = fromJust $ M.lookup "botao2" txts
        let sel = getSelectedMenuButton gs

        return $ Pictures [
            pausa,
            Translate 7 55     $ if sel == 0 then botao2 else botao,
            Translate 2 55     $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_resume"),

            Translate 7 (-65)  $ if sel == 1 then botao2 else botao,
            Translate 7 (-65)  $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_save"),

            Translate 7 (-195) $ if sel == 2 then botao2 else botao,
            Translate 7 (-195) $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_options"),
            
            Translate 7 (-325) $ if sel == 3 then botao2 else botao,
            Translate 7 (-325) $ drawLimitedText chopsic (1,1) (150,32) (getLocalizedStringP locale "menu_exit")
            ]
    | isInMenu gs && getSelectedMenu gs == MID_Save = do
        sm <- drawSaveMenu (gs,cd) False

        return $ Scale (fromIntegral ww / 1000) (fromIntegral wh / 1000) sm
    | isInMenu gs && getSelectedMenu gs == MID_Save_DelAllConf = do
        sm <- drawSaveMenu (gs,cd) True
        sdac <- drawSaveDelAllConf (gs,cd)

        return $ Scale (fromIntegral ww / 1000) (fromIntegral wh / 1000) $ Pictures [
                sm,
                sdac
            ]
    | isInMenu gs && getSelectedMenu gs == MID_Save_Active = do
        sm <- drawSaveMenu (gs,cd) True
        sa <- drawSaveActive (gs,cd)

        return $ Scale (fromIntegral ww / 1000) (fromIntegral wh / 1000) $ Pictures [
                sm,
                sa
            ]

    | otherwise = return blank
    where (ww,wh) = displayDimension

drawSaveMenu :: (GameState, CacheData) -> Bool -> IO Picture
drawSaveMenu (gs,cd) snapshotted = do
    let locale = getLocale gs
    let txts = getTextures gs
    let fonts = getFonts gs

    let chopsic = fromJust $ M.lookup "chopsic" fonts

    let bg = fromJust $ M.lookup "menu_layout" txts
    let sign_active = fromJust $ M.lookup "save_sign_active" txts
    let sign_inactive = fromJust $ M.lookup "save_sign_inactive" txts
    let sign_selection = fromJust $ M.lookup "save_sign_selected" txts

    let sel = getSelectedMenuButton gs

    let saveData = saves cd
    let saveFileStats = map isJust saveData
    let saveDataHeaders = map (\x -> if isNothing x then Nothing else Just $ header (fromJust x)) saveData
    let _selectedSave = saveData !! sel
    let selectedSave = fromMaybe defaultSaveData _selectedSave

    let signY = [251,64,(-115),(-298)]
    let textYOffset = 39

    return $ Pictures [
        bg,

        -- Save signs
        Translate (182) (signY !! 0) $ if saveFileStats !! 0 then sign_active else sign_inactive,
        Translate (182) (signY !! 1) $ if saveFileStats !! 1 then sign_active else sign_inactive,
        Translate (182) (signY !! 2) $ if saveFileStats !! 2 then sign_active else sign_inactive,
        Translate (182) (signY !! 3) $ if saveFileStats !! 3 then sign_active else sign_inactive,
        Translate (166) (-454) $ Scale 0.35 0.44 sign_active,
        Translate (383) (-454) $ Scale 0.35 0.44 sign_active,

        -- Sign text
        let sy = signY !! 0 in Translate (-50) (sy + textYOffset) $ Scale 1.4 1.4 $ strToPic chopsic "Save 1" AlignCenter,
        let {i = 0; sy = signY !! i; he = saveDataHeaders !! i; h = fromJust he;} in 
            Translate (-60) (sy + textYOffset - 70) $ Scale 0.6 0.6 $ 
                strToPic chopsic (
                    getLocalizedStringP locale "menu_save_lastModifiedAt" 
                    ++ ": " ++ (if isNothing he then "N/A" else (formatDate . makeTime) (mdate h))
                ) AlignCenter,

        let sy = signY !! 1 in Translate (-50) (sy + textYOffset) $ Scale 1.4 1.4 $ strToPic chopsic "Save 2" AlignCenter,
        let {i = 1; sy = signY !! i; he = saveDataHeaders !! i; h = fromJust he;} in 
            Translate (-60) (sy + textYOffset - 70) $ Scale 0.6 0.6 $ 
                strToPic chopsic (
                    getLocalizedStringP locale "menu_save_lastModifiedAt" 
                    ++ ": " ++ (if isNothing he then "N/A" else (formatDate . makeTime) (mdate h))
                ) AlignCenter,

        let sy = signY !! 2 in Translate (-50) (sy + textYOffset) $ Scale 1.4 1.4 $ strToPic chopsic "Save 3" AlignCenter,
        let {i = 2; sy = signY !! i; he = saveDataHeaders !! i; h = fromJust he;} in 
            Translate (-60) (sy + textYOffset - 70) $ Scale 0.6 0.6 $ 
                strToPic chopsic (
                    getLocalizedStringP locale "menu_save_lastModifiedAt" 
                    ++ ": " ++ (if isNothing he then "N/A" else (formatDate . makeTime) (mdate h))
                ) AlignCenter,

        let sy = signY !! 3 in Translate (-50) (sy + textYOffset) $ Scale 1.4 1.4 $ strToPic chopsic "Save 4" AlignCenter,
        let {i = 3; sy = signY !! i; he = saveDataHeaders !! i; h = fromJust he;} in 
            Translate (-60) (sy + textYOffset - 70) $ Scale 0.6 0.6 $ 
                strToPic chopsic (
                    getLocalizedStringP locale "menu_save_lastModifiedAt" 
                    ++ ": " ++ (if isNothing he then "N/A" else (formatDate . makeTime) (mdate h))
                ) AlignCenter,

        -- Save Details
        if elem sel [0,1,2,3] then Pictures [
            Translate (-455) (-10) $ Scale 0.7 0.7 $ 
                strToPic chopsic (getLocalizedStringP locale "menu_save_details") AlignCenter,

            Translate (-452) (-90) $ Scale 0.45 0.45 $ 
                strToPic chopsic (
                    getLocalizedStringP locale "menu_save_createdAt" ++ ": " 
                    ++ (formatDate . makeTime) ((cdate . header) selectedSave)
                ) AlignCenter,
            Translate (-452) (-113) $ Scale 0.45 0.45 $ 
                strToPic chopsic (
                    getLocalizedStringP locale "menu_save_modifiedAt" ++ ": " 
                    ++ (formatDate . makeTime) ((mdate . header) selectedSave)
                ) AlignCenter,

            Translate (-452) (-155) $ Scale 0.45 0.45 $ 
                strToPic chopsic (
                    getLocalizedStringP locale "menu_save_highscore" ++ ": " 
                    ++ show (highScore $ gpd $ Save.playerData $ body selectedSave)
                ) AlignCenter
            ]
        else Blank,

        -- Seleted Hover
        let sel = if snapshotted then snd3 $ lastMenuSnapshot cd else getSelectedMenuButton gs in
        case sel of {
            sel
                | elem sel [0,1,2,3] -> Translate (182) (signY !! sel) sign_selection
                | sel == 4 -> Translate (166) (-454) $ Scale 0.35 0.44 $ sign_selection
                | sel == 5 -> Translate (383) (-454) $ Scale 0.35 0.44 $ sign_selection
            }, 

        -- Clear saves text
        let {
            w = 209;
            clearText = getLocalizedStringP locale "menu_save_clear";
            (sw,sh) = trueStringSize chopsic clearText;
            es = max 0 (w - sw);
            x = 166 + (w/2) - sw - (es/3)
        } in 
        Translate x (-450) $ 
            Scale 1 1 $ strToPic chopsic clearText AlignCenter,  

        -- Exit text
        Translate (355) (-450) $ Scale 1 1 $ strToPic chopsic (getLocalizedStringP locale "menu_exit") AlignCenter
        ]

drawSaveDelAllConf (gs,cd) = do
    let locale = getLocale gs
    let txts = getTextures gs
    let fonts = getFonts gs

    let chopsic@(FontData _ (x,y) ot) = fromJust $ M.lookup "chopsic" fonts
    let sign_active = fromJust $ M.lookup "save_sign_active" txts
    let sign_shadow = fromJust $ M.lookup "save_sign_shadow" txts
    let sign_selection = fromJust $ M.lookup "save_sign_selected" txts

    let sel = getSelectedMenuButton gs
    let mat = menuAditionalText cd

    return $ Pictures [
        Scale 1 3 sign_active,
        let {
            warnText = if mat !! 0 == "0" then 
                    getLocalizedStringP locale "menu_save_delAll_conf" 
                else
                    printf (getLocalizedStringP locale "menu_save_del_conf") (read $ mat !! 1 :: Int)
            ;
            (ltSize,_) = trueStringSize chopsic warnText;
            scale = 0.8;
        } in
            Translate (-(ltSize * scale / 2)) 100 $ Scale scale scale $ strToPic chopsic warnText AlignCenter,
        
        Scale 0.48 0.45 $ sign_shadow,
        Scale 0.5 0.5 $ sign_active,
        if sel == 0 then Scale 0.5 0.5 $ sign_selection else Blank,
        let {
            warnText = getLocalizedStringP locale "menu_no";
            (ltSize,_) = trueStringSize chopsic warnText;
            scale = 2;
        } in
            Translate (-(ltSize * scale / 3)) 5 $ Scale scale scale $ strToPic chopsic warnText AlignCenter,

        Translate 0 (-100) $ Scale 0.48 0.45 $ sign_shadow,
        Translate 0 (-100) $ Scale 0.5 0.5 $ sign_active,
        Translate 0 (-100) $ if sel == 1 then Scale 0.5 0.5 $ sign_selection else Blank,
        let {
            warnText = getLocalizedStringP locale "menu_yes";
            (ltSize,_) = trueStringSize chopsic warnText;
            scale = 2;
        } in
            Translate (-(ltSize * scale / 3)) (-95) $ Scale scale scale $ strToPic chopsic warnText AlignCenter
        ]

drawSaveActive (gs,cd) = do
    let locale = getLocale gs
    let txts = getTextures gs
    let fonts = getFonts gs

    let chopsic@(FontData _ (x,y) ot) = fromJust $ M.lookup "chopsic" fonts
    let sign_active = fromJust $ M.lookup "save_sign_active" txts
    let sign_shadow = fromJust $ M.lookup "save_sign_shadow" txts
    let sign_selection = fromJust $ M.lookup "save_sign_selected" txts

    let sel = getSelectedMenuButton gs
    let mat = menuAditionalText cd

    return $ Pictures [
        Scale 1 5 sign_active,
        let {
            saveText = capitalizeWord (getLocalizedStringP locale "menu_save_save") ++ " " ++ show (read (mat !! 0) + 1);
            (ltSize,_) = trueStringSize chopsic saveText;
            scale = 2;
        } in Translate (-(ltSize * scale / 2)) (250) $ Scale scale scale $ strToPic chopsic saveText AlignCenter,

        Translate 0 100 $ 
            drawSaveButton (txts,chopsic) (0,0) (1.25,1.25) (200,64) 
                (getLocalizedStringP locale "menu_save_load_verb") True (sel == 0) False,
        Translate 0 (0) $ 
            drawSaveButton (txts,chopsic) (0,0) (1.25,1.25) (200,64) 
                (getLocalizedStringP locale "menu_save_save_verb") True (sel == 1) (not $ isInGameMenu gs),
        Translate 0 (-100) $ 
            drawSaveButton (txts,chopsic) (0,0) (1.25,1.25) (200,64) 
                (getLocalizedStringP locale "menu_save_delete_verb") True (sel == 2) False,
        Translate 0 (-200) $ 
            drawSaveButton (txts,chopsic) (0,0) (1.25,1.25) (200,64) 
                (getLocalizedStringP locale "menu_exit") True (sel == 3) False
        ]

drawSaveButton (txts,font) (px,py) (tsx,tsy) (msx,msy) txt shadowed selected disabled =
    Pictures [
        if shadowed then Translate px py $ Scale 0.48 0.45 $ sign_shadow else Blank,
        Translate px py $ Scale 0.5 0.5 $ if disabled then sign_inactive else sign_active,
        if selected then Translate px py $ Scale 0.5 0.5 $ sign_selection else Blank,
        let {
                (ltxSize,ltySize) = trueStringSize font txt;
                (sw,sh) = (
                    --1,1
                    if msx / ltxSize > 1 then tsx else tsx * (msx / ltxSize),
                    if msy / ltySize > 1 then tsy else tsy * (msy / ltySize)
                    )
            } in
                Translate (16 -(ltxSize * sw / 2)) (py + 5) $ Scale sw sh $ strToPic font txt AlignCenter
        ]
    where
        sign_active = fromJust $ M.lookup "save_sign_active" txts
        sign_inactive = fromJust $ M.lookup "save_sign_inactive" txts
        sign_shadow = fromJust $ M.lookup "save_sign_shadow" txts
        sign_selection = fromJust $ M.lookup "save_sign_selected" txts

drawLimitedText font (tsx,tsy) (msx,msy) txt = Translate (16 -(ltxSize * sw / 2)) 5 $ 
    Scale sw sh $ strToPic font txt AlignCenter
    where
        (ltxSize,ltySize) = trueStringSize font txt;
        (sw,sh) = (
            if msx / ltxSize > 1 then tsx else tsx * (msx / ltxSize),
            if msy / ltySize > 1 then tsy else tsy * (msy / ltySize)
            )

drawGame (gs,cd) = do 
    _lines <- mapM (drawLine txts mapa) (reverse ol)
    let lines = Pictures $ snd $ mapAccumL (\c l -> (c+1,Translate 0 (c*yy- (0.5*(fromIntegral (length _lines)-1))*yy) l)) 0 _lines

    jogador <- drawPlayer gs
    menuTextures <- drawMenus (gs,cd)
    let score = drawScore gs Nothing

    (wx,wy) <- getScreenSize
    let (rwx,rwy) = (fromIntegral (wx - ww) / 4, fromIntegral (wy - hh) / 4)

    let borders = Pictures [
            Translate 0 ((fromIntegral hh / 2) + 10) $ color borderColor $ rectangleSolid (fromIntegral ww) 20,
            Translate 0 (-(fromIntegral hh / 2) - 10) $ color borderColor $ rectangleSolid (fromIntegral ww) 20
            ]

    if getClock1Value gs < round (9*fps) then 
        return $ Pictures ([
            Translate 0 (-getClock2Value gs) (Pictures ([lines, jogador]))] 
            ++ [menuTextures,score,borders]
        )
    else return $ Pictures [lines, jogador, menuTextures, score, borders]
    
    where
        (ww,hh) = displayDimension
        (Game _ (Map _ off ol)) = RP.game $ fromJust $ getLevelData gs
        txts = getTextures gs
        mapa = fromJust $ getGameMap gs
        (_,yy) = getTileSize mapa

drawLine :: M.Map String Picture -> Map -> (Terreno, [Obstaculo]) -> IO Picture
drawLine txts m@(Map w off l) (t, ol) = do
    let (ww,wh) = displayDimension
    let (xx,yy) = getTileSize m

    let galinha = fromJust $ M.lookup "chicken" txts
    let nenhumrelva = fromJust $ M.lookup "nenhumrelva" txts
    let nenhumrio = fromJust $ M.lookup "nenhumrio" txts
    let nenhumestrada = fromJust $ M.lookup "nenhumestrada" txts
    let arvore = fromJust $ M.lookup "arvore" txts
    let carro = fromJust $ M.lookup "carro" txts
    let carro2 = fromJust $ M.lookup "carro2" txts

    let tronco = fromJust $ M.lookup "tronco" txts
    let tronco_left = fromJust $ M.lookup "tronco_left" txts
    let tronco_right = fromJust $ M.lookup "tronco_right" txts
    let tronco_full = fromJust $ M.lookup "tronco_full" txts

    let bg = Pictures . snd $ mapAccumL (\cc x ->  (cc+1, Translate (-(fromIntegral ww/2)+(xx/2)+(xx*cc)) 0 $ case t of { 
            Relva -> Scale (xx/112) (yy/112) nenhumrelva;
            Rio v -> Scale (xx/112) (yy/112) nenhumrio;  
            Estrada v -> Scale (xx/112) (yy/112) nenhumestrada;
        })) 0 ol

    let fg = Pictures . snd $ mapAccumL (\cc x ->  (cc+1, Translate (-(fromIntegral ww/2)+(xx/2)+(xx*cc)) 0 $ case x of { 
            Nenhum -> Blank;
            Arvore -> Scale (xx/112) (yy/112) arvore;
            Carro -> case t of {

                Estrada v -> 
                    let car = Translate (((fromIntegral v / fps) * max 1 off) * xx) 0 $ 
                            if v >= 0 then Scale (xx/112) (yy/112) carro else Scale (xx/112) (yy/112) carro2
                    in Pictures [
                        if round cc + v >= w || round cc + v <= 0 then 
                            Translate (-fromIntegral ww * signum (fromIntegral v)) 0 $ car 
                            else car
                        ,car
                    ];
                    
            };
            Tronco -> case t of {
                Rio v -> 
                    let log = Translate (((fromIntegral v / fps) * max 1 off) * xx) 0 $ 
                            Scale (xx/112) (yy/112) (case () of {
                                ()
                                    | getRelativeTile (t,ol) (round cc) (-1) == Tronco 
                                        && getRelativeTile (t,ol) (round cc) 1 == Tronco -> tronco_full
                                    | getRelativeTile (t,ol) (round cc) (-1) == Tronco -> tronco_left
                                    | getRelativeTile (t,ol) (round cc) 1 == Tronco -> tronco_right
                                    | otherwise -> tronco
                                    -- | cc == 0 && ol !! (round cc + 1) == Tronco -> tronco_right
                                    -- | cc == 0 -> tronco
                                    -- | ol !! (round cc - 1) == Tronco && ol !! (round cc + 1) == Tronco -> tronco_full
                                    -- | ol !! (round cc - 1) == Tronco -> tronco_left
                                    -- | ol !! (round cc + 1) == Tronco -> tronco_right
                            });
                    in Pictures [
                        if round cc + v >= w || round cc + v <= 0 then 
                            Translate (-fromIntegral ww * signum (fromIntegral v)) 0 $ log
                            else Blank
                        , log
                    ];        
            };
        })) 0 ol

    let img = Translate 0 (yy/2) $ Pictures [bg, fg]
    return img

drawPlayer :: GameState -> IO Picture
drawPlayer gs = do
    let txts = getTextures gs

    let galinha = if isNoClip gs then fromJust $ M.lookup "chickengold" txts else fromJust $ M.lookup "chicken" txts
    let (tw,th) = getTileSize m
    (xx,yy) <- convertPC (x,y) (fromJust $ getGameMap gs)
    let (ox, oy) = (((vx*off)/(fps/8))*tw, -((vy*off)/(fps/8))*th)

    return $ Translate (xx + ox) (yy + oy + (th)) galinha
    where
        m = fromJust $ getGameMap gs
        off = fromJust $ getGamePlayerOffset gs
        (x,y) = fromJust $ getGamePlayerCoords gs
        (vx,vy) = fromJust $ getGamePlayerVelocity gs

drawScore :: GameState -> Maybe Int -> Picture
drawScore gs p
    | isInMenu gs && getSelectedMenu gs == MID_MainMenu = Pictures [
        Translate (0) (-400) score,
        Translate (10) (-400) $ drawLimitedText chopsic (1,1) (128,32) (tPoints)
    ]

    | isInGame gs && not (isPaused gs) = Pictures [
        Translate (fromIntegral ww/2 -75) (fromIntegral wh/2 -25) score,
        Translate (fromIntegral ww/2 -65) (fromIntegral wh/2 -25) $ drawLimitedText chopsic (1,1) (128,32) (tPoints)
    ]

    | isPaused gs = Translate 0 (-370) score

    | otherwise = Blank 
    where 
        (ww,wh) = displayDimension
        txts = getTextures gs
        fonts = getFonts gs
        score = fromJust $ M.lookup "score" txts
        chopsic = fromJust $ M.lookup "chopsic" fonts

        levelPoints = let pts = getPlayerLevelPoints gs in fromMaybe 0 pts
        points = show $ fromMaybe levelPoints p
        tPoints = take (4 - length points) (repeat '0') ++ points





        