module Render.Common where
import Save
import Render.Play
import Tarefa3_v2

data CacheData = CacheData {
    saveNames :: [String],
    saves :: [Maybe SaveData],
    lastMenuSnapshot :: (MenuId,Int,[String]),
    menuAditionalText :: [String],
    allowSave :: Bool,
    keySequence :: String
} deriving (Show)

defaultCacheData = CacheData ["kfcrun_sav1", "kfcrun_sav2", "kfcrun_sav3", "kfcrun_sav4"] [] (MID_MainMenu,0,[]) [] False ""

restoreMenuSnapshot :: (GameState, CacheData) -> (GameState, CacheData)
restoreMenuSnapshot (gs,cd) = 
    (popFromMenuStack $ setSelectedMenuButton (setSelectedMenu gs sm) sb,cd { menuAditionalText = mat })
    where
        (sm,sb,mat) = lastMenuSnapshot cd

convertPC :: (Float,Float) -> Map -> IO (Float,Float)
convertPC (px,py) (Map w off l) = do
    let (ww,wh) = displayDimension
    let fr = fromIntegral
    let (tw,th) = getTileSize (Map w off l)
    let (tr1x, tr1y) = (-(fromIntegral ww/2)+(tw/2) + (tw*px),(fromIntegral wh/2)-(th/2) - (th*py))

    return (tr1x,tr1y)

addToKeySequence cd c = cd { keySequence = (keySequence cd) ++ [c] }

verifyKeySequence (gs,cd)
    | length ks > maxLength = (gs,cd { keySequence = "" })
    | ks == "noclip" = (toggleFlag (toggleFlag gs GodMode) NoClip, cd { keySequence = "" })
    | otherwise = (gs,cd)
    where
        ks = keySequence cd
        maxLength = 6
