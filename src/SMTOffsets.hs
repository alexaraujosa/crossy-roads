module SMTOffsets where

import Data.Map (Map)
import qualified Data.Map as Map
import Render.Backend.Atlas

offsets = Offsets {
    atlas = Map.fromList [("deadmenu", OffsetEntry 0 0 1000 1000),("mainmenu", OffsetEntry 1000 0 1000 1000),("optionsmenu", OffsetEntry 0 1000 1000 1000),("optionsmenup", OffsetEntry 1000 1000 1000 1000),("pausemenu", OffsetEntry 2000 0 1000 1000),("menu_layout", OffsetEntry 2000 1000 1000 1000),("sidebars", OffsetEntry 0 2000 300 1000),("save_sign_shadow", OffsetEntry 300 2000 693 245),("save_sign_active", OffsetEntry 993 2000 597 149),("save_sign_inactive", OffsetEntry 1590 2000 597 149),("save_sign_selected", OffsetEntry 2187 2000 597 149),("arvore", OffsetEntry 2784 2000 112 112),("carro", OffsetEntry 300 2245 112 112),("carro2", OffsetEntry 412 2245 112 112),("chicken", OffsetEntry 524 2245 112 112),("chickengold", OffsetEntry 636 2245 112 112),("nenhumestrada", OffsetEntry 748 2245 112 112),("nenhumrelva", OffsetEntry 860 2245 112 112),("nenhumrio", OffsetEntry 972 2245 112 112),("tronco", OffsetEntry 1084 2245 112 112),("tronco_full", OffsetEntry 1196 2245 112 112),("tronco_left", OffsetEntry 1308 2245 112 112),("tronco_right", OffsetEntry 1420 2245 112 112),("botao", OffsetEntry 993 2149 275 94),("botao2", OffsetEntry 1268 2149 275 94),("score", OffsetEntry 1543 2149 150 50)]
}