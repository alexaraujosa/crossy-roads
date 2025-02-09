{- |
Module      : Save
Description : Módulo de manipulação de ficheiros de Save.
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de manipulação de ficheiros de Save Game. Permite criar, reescrever e ler ficheiros de Save Game.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Save where
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Char8         as BC8
import GHC.Generics
import Data.Binary
import System.Directory
import System.FilePath (pathSeparator)
import Foreign
import LI12223
import Render.Play.Internal (MapType(..), Player(..))
import Render.Play (Game(..))
import Render.Play (Map(..))
import Render.World
import Render.Play
import Util
import Render.World.Level (getGameMap, getPlayerLevelPoints, setPlayerLevelPoints)
import Data.Maybe
import Data.Char

{-
=======================================
                SETTINGS               
=======================================
-}
saveVersion :: Int
saveVersion = 6

saveDirPath :: IO String
saveDirPath = do
    cwd <- getCurrentDirectory
    return $ cwd ++ [pathSeparator] ++ "kfcrun" ++ [pathSeparator] ++ "saves"

defaultSaveData :: SaveData
defaultSaveData = SaveData {
        header = SaveHeader { sig="KR", ver=4, cdate=1, mdate=1, times_saved=1 },
        body = SaveBody { 
            map_type = REGULAR, 
            map_width = 9, 
            map_lines = ml, 
            playerData=SavePlayerData {
                player = p,
                points = 0,
                gpd = GlobalPlayerData {
                    highScore = 123
                }
            } 
        }
    }
    where
        (Map _ _ ml) = defaultWorld 9 5
        p = Player 0 (0,0) (2,5)
    
{-
=======================================
            DATA STRUCTURES            
=======================================
-}

data SaveData = SaveData { header :: SaveHeader, body :: SaveBody } deriving (Generic, Show)
instance Binary SaveData

data SaveHeader = SaveHeader {
    sig         :: String,
    ver         :: Int,
    cdate       :: Integer,
    mdate       :: Integer,
    times_saved :: Int
} deriving (Generic, Show)
instance Binary SaveHeader

data SaveBody = SaveBody {
    map_type  :: MapType,
    map_width :: Int,
    map_lines :: [Line],
    playerData :: SavePlayerData
} deriving (Generic, Show)
instance Binary SaveBody

data SavePlayerData = SavePlayerData {
    player :: Player,
    points :: Int,
    gpd :: GlobalPlayerData
} deriving (Generic, Show)
instance Binary SavePlayerData

{-
=======================================
          CONVERTER FUNCTIONS          
=======================================
-}

toSaveData :: GameState -> IO SaveData
toSaveData gs = do
    curDate <- getTimeSA
    let (Game p (Map w o l)) = game pd

    return $ SaveData {
        header = SaveHeader { sig="KR", ver=saveVersion, cdate=curDate, mdate=curDate, times_saved=1 },
        body = SaveBody {
            map_type=mapType pd, map_width=w, map_lines=l, playerData=SavePlayerData {
                player = p,
                points = fromJust $ getPlayerLevelPoints gs,
                gpd = globalPlayerData gs
            }
        }
    }
    where
        pd = fromJust $ getLevelData gs

fromSaveData :: SaveData -> IO GameState
fromSaveData sd = do
    r <- getTimeSA
    let gameData = body sd
    let pd = playerData (gameData :: SaveBody)
    gs <- defaultGameState (fromIntegral r)

    let ngs = gs {
        levelData = Just $ defaultLevelData {
            mapType = map_type gameData,
            game = Game (player (playerData (gameData :: SaveBody))) (Map (map_width gameData) 0 (map_lines gameData))
            },
        globalPlayerData = gpd (playerData (gameData :: SaveBody))
        }

    let ngs2 = fromJust $ setPlayerLevelPoints ngs (points (pd :: SavePlayerData))

    let pgs = ngs2
    return pgs
{-
=======================================
            HANDLE FUNCTIONS           
=======================================
-}

writeSave :: SaveData -> String -> IO ()
writeSave sd sn = do
    let encodedData = encode sd
    let encoded = encode 'K' <> encode 'R' <> encode (ver (header sd)) <> encodedData

    savePath <- saveDirPath

    createDirectoryIfMissing True savePath
    B.writeFile (savePath ++ [pathSeparator] ++ sn ++ ".dat") (toStrict encoded)

loadSave :: String -> IO SaveData
loadSave sn = do
    savePath <- saveDirPath
    let filePath = savePath ++ [pathSeparator] ++ sn ++ ".dat"
    exists <- doesFileExist filePath

    if exists then do
        fileData <- BL.readFile filePath
        let msig = bsToStr $ BL.take 2 fileData
        let mver = fromIntegral (decode $ BL.drop 5 (BL.take 10 fileData) :: Integer)
        let decoded = decode (BL.drop 10 fileData) :: SaveData
        let saveDecoder
                | msig /= "KR" = error ("Invalid save file - " ++ sn ++ ": not a save file.")
                | mver < saveVersion = error ("Invalid save file - " ++ sn ++ ": save file is from an older version.")
                | mver > saveVersion = error ("Invalid save file - " ++ sn ++ ": save file is from a newer version.")
                | otherwise = 
                    if msig == "KR" && sig (header decoded) == "KR" then
                        if ver (header decoded) == saveVersion then
                            pure decoded
                        else if ver (header decoded) > saveVersion then
                            error ("Invalid save file - " ++ sn ++ ": save file is from a newer version.")
                        else if ver (header decoded) < saveVersion then
                            error ("Invalid save file - " ++ sn ++ ": save file is from an older version.")
                        else
                            pure decoded
                    else error ("Invalid save file - " ++ sn ++ ": not a save file.")

        saveDecoder
    else error ("Could not find save file with name: " ++ sn)

loadGame :: SaveData -> Game
loadGame sd = Game p (Map mw 0 ml)
    where
        p = player (playerData (body sd :: SaveBody))
        mw = map_width (body sd)
        ml = map_lines (body sd)

{-
=======================================
        PROCESSING FUNCTIONS           
=======================================
-}

bsToStr :: BLI.ByteString -> String
bsToStr = map (chr . fromEnum) . BL.unpack

-- https://stackoverflow.com/a/8552723
toStrict :: BL.ByteString -> B.ByteString
toStrict BLI.Empty = B.empty
toStrict (BLI.Chunk c BLI.Empty) = c
toStrict lb = BI.unsafeCreate len $ go lb
  where
    len = BLI.foldlChunks (\l sb -> l + B.length sb) 0 lb

    go  BLI.Empty                   _   = return ()
    go (BLI.Chunk (BI.PS fp s l) r) ptr =
        withForeignPtr fp $ \p -> do
            BI.memcpy ptr (p `plusPtr` s) (fromIntegral l)
            go r (ptr `plusPtr` l)
