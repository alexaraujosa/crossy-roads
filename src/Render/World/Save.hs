{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Render.World.Save where
    
import Control.Exception ( SomeException, try )
import Save ( loadSave, saveDirPath, SaveData )
import System.Directory ( doesFileExist, removeFile )
import System.FilePath ( pathSeparator )
import Control.Monad ( when )

loadSaves = do
    saveDir <- saveDirPath
    let saveNames = ["kfcrun_sav1", "kfcrun_sav2", "kfcrun_sav3", "kfcrun_sav4"]
    let saveFilePaths = map (\x -> saveDir ++ [pathSeparator] ++ x ++ ".dat") saveNames
    saveFileStats <- mapM doesFileExist saveFilePaths

    saveData <- mapM (\x -> 
        if saveFileStats !! x then do
            trySaveData <- try (loadSave (saveNames !! x)) :: IO (Either SomeException SaveData)
            return $ case trySaveData of
                Left _ -> Nothing
                Right sd -> Just sd
        else return Nothing
        ) [0..length saveFilePaths - 1]

    return saveData

deleteSaves = do
    saveDir <- saveDirPath
    let saveNames = ["kfcrun_sav1", "kfcrun_sav2", "kfcrun_sav3", "kfcrun_sav4"]
    let saveFilePaths = map (\x -> saveDir ++ [pathSeparator] ++ x) saveNames
    let cSaveFilePaths = map (++ ".dat") saveFilePaths
    saveFileStats <- mapM doesFileExist cSaveFilePaths
    let eSaveFilePaths = map snd $ filter fst $ zip saveFileStats cSaveFilePaths


    mapM removeFile eSaveFilePaths

deleteSave i = do
    saveDir <- saveDirPath
    let saveNames = ["kfcrun_sav1", "kfcrun_sav2", "kfcrun_sav3", "kfcrun_sav4"]
    let saveName = saveDir ++ [pathSeparator] ++ (saveNames !! i) ++ ".dat"
    sne <- doesFileExist saveName

    when sne $ removeFile saveName