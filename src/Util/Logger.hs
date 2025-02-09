{- |
Module      : Logger
Description : Módulo de logging.
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de logging para mensagens.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Util.Logger where

import System.Directory (getCurrentDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath (pathSeparator)
import Data.List (elemIndex)
import Data.Time.Format (formatTime)
import Util (getTimeSA)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time (defaultTimeLocale)
import qualified Util.Logger.Colors as LC
import Prelude hiding (error)

{-
=======================================
                SETTINGS               
=======================================
-}

logDirPath :: IO String
logDirPath = do
    cwd <- getCurrentDirectory
    return $ cwd ++ [pathSeparator] ++ "kfcrun" ++ [pathSeparator] ++ "logs"

{-
=======================================
                DEFAULTS               
=======================================
-}
defaultLogData :: LogData
defaultLogData = LogData {colors = defaultLogColorData, colored = True}

defaultLogColorData :: LogColorData
defaultLogColorData = LogColorData { info = LC.blue, warn = LC.yellow, error = LC.red, debug = LC.magenta }

{-
=======================================
                TYPES                  
=======================================
-}

data Level = Info | Warn | Error | Debug deriving (Eq,Show)

data LogData = LogData {
    colors :: LogColorData,
    colored :: Bool
}

data LogColorData = LogColorData {
    info :: LC.Color,
    warn :: LC.Color,
    error :: LC.Color,
    debug :: LC.Color
}

type Logger = String -> IO ()    -- ^ Representa uma instância de um Logger de Nível
type LoggerGen = Level -> Logger -- ^ Representa uma instância de um Logger
instance Show LoggerGen where
    show _ = "[LoggerGen]"

{-
=======================================
              PROCESSORS             
=======================================
-}

{- |
 A função @prepareLog@, recebendo uma configuração de log, um nível de log e uma mensagem, retorna uma mensagem de log
formatada pronta para ser utilizada pelas funções de logging.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
prepareLog :: LogData -> Level -> String -> IO String
prepareLog ld lvl msg = do
    let lvli = case lvl of { Info -> 0; Warn -> 1; Error -> 2; Debug -> 3; } :: Int
    let lvlPref = ["INFO", "WARN", "ERROR", "DEBUG"] !! lvli
    let color = if colored ld then 
            case lvl of {
                Info -> info (colors ld);
                Warn -> warn (colors ld);
                Error -> error (colors ld);
                Debug -> debug (colors ld);
            }
        else id

    dtMs <- getCurrentTime
    let datetime = formatTime defaultTimeLocale "%FT%T%QZ" dtMs
    let lmsg = color $ "[" ++ lvlPref ++ "] " ++ datetime ++ " - " ++ msg ++ "\n"

    return lmsg

{- |
 A função @loggerFS@, recebendo uma configuração de logs e um nome de ficheiro de log, retorna uma função que, dado o nível
do log e a sua mensagem, executa o log para o ficheiro de log especificado.
-}
loggerFS :: LogData -> String -> LoggerGen --Level -> String -> IO ()
loggerFS ld lf lvl msg = do
    dirPath <- logDirPath
    createDirectoryIfMissing True dirPath

    let logPath = dirPath ++ [pathSeparator] ++ lf ++ ".log"
    lmsg <- prepareLog ld{ colored=False } lvl msg

    appendFile logPath lmsg

    return ()

{- |
 A função @loggerSB@, recebendo uma configuração de log, retorna uma função que, dado o nível do log e a sua mensagem, 
executa o log para o stdout. 
-}
loggerSB :: LogData -> LoggerGen --Level -> String -> IO ()
loggerSB ld lvl msg = do
    lmsg <- prepareLog ld lvl msg
    putStr lmsg

{- |
 A função @loggerMixed@, recebendo uma configuração de logs e um nome de ficheiro de log, retorna uma função que, dado o nível
do log e a sua mensagem, executa o log tanto para o stdout tanto como para o ficheiro de log especificado.
-}
loggerMixed :: LogData -> String -> LoggerGen --Level -> String -> IO ()
loggerMixed ld lf lvl msg = do
    loggerFS ld lf lvl msg
    loggerSB ld lvl msg

{- |
 A função @makeLogs@, recebendo um Logger parcialmente aplicado a dois argumentos do seu término, retorna um tuplo de nível 4
contendo Loggers derivados do Logger original parcialmente aplicados a cada nível de log (info, warn, error, debug).
-}
makeLogs :: LoggerGen -> (Logger, Logger, Logger, Logger)
makeLogs l = (
    l Info, l Warn, l Error, l Debug
    )