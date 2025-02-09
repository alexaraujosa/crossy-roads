{- |
Module      : Locale
Description : Módulo de tradução.
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de tradução do texto ao longo de todo o projeto.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Locale where

import GHC.Generics
import Data.Aeson
    ( decode,
      defaultOptions,
      genericToEncoding,
      FromJSON,
      ToJSON(toEncoding) )
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (pathSeparator)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Util.Record (getField)
import Util (throws)
import Control.Exception (ErrorCall, catch, evaluate)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

{-
=======================================
                SETTINGS               
=======================================
-}
localeVersion :: Int
localeVersion = 2

localeDirPath :: IO String
localeDirPath = do
    cwd <- getCurrentDirectory
    return $ cwd ++ [pathSeparator] ++ "kfcrun" ++ [pathSeparator] ++ "lang" ++ [pathSeparator]

data LocaleId = EN_US | PT_PT deriving (Eq, Show)

{-
=======================================
        LOCALIZATION PROCESSORS        
=======================================
-}

getCachedLocale :: LocaleId -> IO Locale
getCachedLocale EN_US = do en_US
getCachedLocale PT_PT = do pt_PT

-- {- |
--  A função @loadLocale@ carrega um ficheiro de tradução para a memória para ser utilizado.
-- -}
-- loadLocale :: LocaleId -> IO Locale
-- loadLocale EN_US = en_US
-- loadLocale PT_PT = undefined

{- |
 A função @getLocalizedString@, dado um 'Locale', traduz a chave de tradução para a linguagem do mesmo Locale.
-}
getLocalizedString :: Locale -> String -> IO String
getLocalizedString l k = do
    let lf = getField k (locales l) :: String
    llf <- catch (evaluate lf) handler

    if null llf then
        pure $ error ("Could not translate key '" ++ k ++ "': Unexistent or invalid localization key.")
    else pure lf
    where
        handler :: ErrorCall -> IO String
        handler _ = error ("Could not translate key '" ++ k ++ "': Unexistent localization key.")

getLocalizedStringP :: Locale -> String -> String
getLocalizedStringP l k
    | null lf = error ("Could not translate key '" ++ k ++ "': Unexistent or invalid localization key.")
    | otherwise = lf
    where
        lf = getField k (locales l) :: String 
        

{- |
    Lista de locales disponíveis universalmente.
-}

#ifdef DEBUG

-- | Representa um Locale.
data Locale = Locale {
    version :: Int,
    lang :: String,
    locales :: Locales
} deriving (Generic, Show, Data, Typeable)

#else

-- | Representa um Locale.
data Locale = Locale {
    version :: Int,
    lang :: String,
    locales :: Locales
} deriving (Generic, Data, Typeable)
instance Show Locale where
    show l = "[Locale " ++ lang l ++ "]"
    
#endif

instance ToJSON Locale where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Locale

-- | Representa a lista de locales disponíveis e requeridos para um locale.
data Locales = Locales {
    menu_yes :: String,
    menu_no :: String,
    menu_resume :: String,
    menu_respawn :: String,

    menu_exit :: String,
    menu_play :: String,

    menu_options :: String,
    menu_options_english :: String,
    menu_options_portugues :: String,

    menu_save :: String,
    menu_save_save :: String,
    menu_save_save_verb :: String,
    menu_save_load_verb :: String,
    menu_save_delete_verb :: String,
    menu_save_delAll_conf :: String,
    menu_save_del_conf :: String,
    menu_save_createdAt :: String,
    menu_save_modifiedAt :: String,
    menu_save_lastModifiedAt :: String,
    menu_save_highscore :: String,
    menu_save_totalCoins :: String,
    menu_save_details :: String,
    menu_save_clear :: String
} deriving (Generic,Show, Data, Typeable)
instance ToJSON Locales where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Locales

ensureStructure :: Locale -> Bool
ensureStructure l = version l == localeVersion

{-
=======================================
            LOCALE LOADERS             
=======================================
-}

-- | A função @loadLocale@ carrega um Locale a partir de um ficheiro.
loadLocale :: LocaleId -> IO Locale
loadLocale l = do
    let lid = ["en_US","pt_PT"] !! fromJust (elemIndex l [EN_US,PT_PT])

    localePath <- localeDirPath
    let filePath = localePath ++ lid ++ ".json"
    exists <- doesFileExist filePath

    if exists then do
        input <- BL.readFile filePath
        let localeData = decode input :: Maybe Locale

        case localeData of {
            Nothing -> error ("Could not load language '" ++ lid ++ "': could not parse JSON.");
            Just l -> 
                if ensureStructure l then pure l 
                else error ("Could not load language '" ++ lid ++ "': locale version does not match current version.");
        }
    else error ("Could not load language '" ++ lid ++ "': Language file not found.")

en_US :: IO Locale
en_US = do loadLocale EN_US

pt_PT :: IO Locale
pt_PT = do loadLocale PT_PT
